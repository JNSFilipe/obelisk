;;; ytube.el --- Search and watch YouTube inside Emacs -*- lexical-binding: t; -*-

;; This file has no dependencies beyond Emacs itself.  It does not use Doom
;; macros, so it can be loaded from any configuration, byte-compiled and tested
;; on its own.  Playback needs a graphical Emacs built with xwidgets (WebKit).

;;; Commentary:

;; `ytube-search' prompts for a query and lists the results with
;; `completing-read', so Vertico (or any completion UI) shows them.  Each row
;; carries the title, an annotation with channel, duration, views and age,
;; and, in graphical frames, a thumbnail.  Choosing a result plays it in an
;; Emacs buffer through an xwidget-webkit browser.
;;
;; Search backends:
;;
;;   yt-dlp   used when `ytube-yt-dlp-program' is installed.  It tracks YouTube
;;            changes best, but its flat search omits upload dates.
;;   builtin  YouTube's own web search endpoint (the one youtube.com calls),
;;            reached with url.el.  No external program or API key.
;;
;; With `ytube-search-backend' set to `auto', yt-dlp is preferred and the
;; builtin backend is the fallback when yt-dlp is missing or fails.
;; Thumbnails come from i.ytimg.com by video ID, so they work with either.
;;
;; Playback: YouTube refuses an embed loaded as a top-level page (Error 153,
;; no referring origin).  ytube therefore serves a tiny wrapper page from an
;; Emacs network process on localhost; the wrapper embeds the player through
;; YouTube's IFrame API, and that page is what the xwidget shows.  The server
;; only answers GET /watch?v=<11-character ID> and holds no other state.
;;
;; Player keys (`ytube-player-mode'), mirroring YouTube's own:
;;
;;   k, SPC   play/pause        j, l     seek -/+10 seconds
;;   <left>, <right>  seek -/+5 seconds  m    mute
;;   <up>, <down>     volume +/-10       <, > slower/faster
;;   w   copy the watch URL   o   open in the external browser
;;   s   new search           q   bury     Q   kill the player
;;
;; Under Evil the player buffer uses Emacs state so these keys reach it.

;;; Code:

(require 'cl-lib)
(require 'format-spec)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-http)

(declare-function xwidget-insert "xwidget" (pos type title width height &optional args related))
(declare-function xwidget-put "xwidget.c" (xwidget propname value))
(declare-function xwidget-at "xwidget" (pos))
(declare-function xwidget-live-p "xwidget.c" (xwidget))
(declare-function xwidget-webkit-goto-uri "xwidget.c" (xwidget uri))
(declare-function xwidget-webkit-execute-script "xwidget.c" (xwidget script &optional callback))
(declare-function xwidget-webkit-adjust-size-to-window "xwidget" (xwidget &optional window))
(declare-function xwidget-webkit-callback "xwidget" (xwidget xwidget-event-type))
(declare-function xwidget-webkit-mode "xwidget")
(declare-function set-xwidget-query-on-exit-flag "xwidget.c" (xwidget flag))
(declare-function evil-emacs-state "evil-states" (&optional arg))
(declare-function browse-url-default-browser "browse-url" (url &rest args))

(defvar url-http-end-of-headers)
(defvar url-http-response-status)
(defvar xwidget-webkit-buffer-name-format)
(defvar evil-local-mode)


;;; --- Options ----------------------------------------------------------------

(defgroup ytube nil
  "Search and watch YouTube inside Emacs."
  :group 'multimedia
  :prefix "ytube-")

(defcustom ytube-search-backend 'auto
  "How `ytube-search' queries YouTube.
`auto' uses yt-dlp when installed and falls back to the builtin
backend when it is missing or fails.  `builtin' calls YouTube's web
search endpoint directly, needing nothing beyond Emacs."
  :type '(choice (const :tag "yt-dlp if installed, else builtin" auto)
                 (const :tag "yt-dlp only" yt-dlp)
                 (const :tag "Builtin (url.el)" builtin)))

(defcustom ytube-yt-dlp-program "yt-dlp"
  "The yt-dlp executable, used for search when available."
  :type 'string)

(defcustom ytube-max-results 20
  "Maximum number of search results.
The builtin backend returns at most one page, about 20 videos."
  :type 'natnum)

(defcustom ytube-search-timeout 15
  "Seconds to wait for the builtin backend before giving up."
  :type 'number)

(defcustom ytube-thumbnails t
  "Non-nil shows video thumbnails in graphical frames.
See also `ytube-toggle-thumbnails'."
  :type 'boolean)

(defcustom ytube-thumbnail-height nil
  "Thumbnail height in pixels; nil means two text lines."
  :type '(choice (const :tag "Two lines" nil) natnum))

(defcustom ytube-embed-host "https://www.youtube-nocookie.com"
  "Host serving the embedded player.
The nocookie host keeps YouTube from setting tracking cookies until
playback starts.  \"https://www.youtube.com\" also works."
  :type 'string)

(defcustom ytube-autoplay t
  "Non-nil starts playback as soon as the player loads."
  :type 'boolean)

(defcustom ytube-reuse-player t
  "Non-nil plays new videos in the existing player buffer.
Nil opens a separate player buffer per video."
  :type 'boolean)

(defcustom ytube-player-buffer-name-format "*YouTube: %T*"
  "Player buffer name; %T is replaced by the video title."
  :type 'string)

(defface ytube-channel '((t :inherit font-lock-type-face))
  "Face for channel names in search results.")

(defface ytube-metadata '((t :inherit completions-annotations))
  "Face for duration, views and age in search results.")


;;; --- Videos -----------------------------------------------------------------

(cl-defstruct (ytube-video (:constructor ytube-video-create)
                           (:copier nil))
  "A search result."
  id title channel duration views published)

(defconst ytube--id-regexp "[A-Za-z0-9_-]\\{11\\}"
  "YouTube video IDs are 11 URL-safe base64 characters.")

(defun ytube--valid-id-p (id)
  "Return non-nil when ID is a well-formed video ID."
  (and (stringp id)
       (string-match-p (concat "\\`" ytube--id-regexp "\\'") id)))

(defun ytube--extract-id (string)
  "Return the video ID in STRING, a watch/short/embed URL or a bare ID."
  (let ((string (string-trim string)))
    (cond
     ((ytube--valid-id-p string) string)
     ((string-match (concat "\\(?:[?&]v=\\|youtu\\.be/\\|/shorts/\\|/embed/\\|/live/\\)"
                            "\\(" ytube--id-regexp "\\)\\(?:[^A-Za-z0-9_-]\\|\\'\\)")
                    string)
      (match-string 1 string)))))

(defun ytube--watch-url (id)
  "Return the youtube.com watch URL of video ID."
  (concat "https://www.youtube.com/watch?v=" id))

(defun ytube--format-duration (seconds)
  "Format SECONDS as H:MM:SS or M:SS; nil when SECONDS is not a number."
  (when (numberp seconds)
    (let* ((s (truncate seconds))
           (h (/ s 3600)) (m (/ (% s 3600) 60)) (sec (% s 60)))
      (if (> h 0)
          (format "%d:%02d:%02d" h m sec)
        (format "%d:%02d" m sec)))))

(defun ytube--format-views (count)
  "Format COUNT views compactly, as YouTube does (\"97K views\")."
  (when (numberp count)
    (let* ((units '((1e9 . "B") (1e6 . "M") (1e3 . "K")))
           (unit (seq-find (lambda (u) (>= count (car u))) units)))
      (if (not unit)
          (format "%d view%s" count (if (= count 1) "" "s"))
        (let* ((value (/ count (car unit)))
               (text (if (< value 10)
                         (string-remove-suffix ".0" (format "%.1f" (/ (ffloor (* value 10)) 10)))
                       (format "%d" (truncate value)))))
          (format "%s%s views" text (cdr unit)))))))


;;; --- Search: builtin --------------------------------------------------------

(defconst ytube--innertube-url
  "https://www.youtube.com/youtubei/v1/search?prettyPrint=false"
  "YouTube's web search endpoint (InnerTube).")

(defvar ytube--innertube-client-version "2.20241001.00.00"
  "WEB client version sent to InnerTube.
YouTube accepts older versions; bump this if searches start failing.")

(defun ytube--innertube-request (query)
  "Return the UTF-8 JSON body searching InnerTube for videos matching QUERY."
  (let ((json (json-serialize
               `((context
                  . ((client . ((clientName . "WEB")
                                (clientVersion . ,ytube--innertube-client-version)
                                (hl . "en")
                                (gl . "US")))))
                 (query . ,query)
                 ;; Search filter "Type: Video" (no channels or playlists).
                 (params . "EgIQAQ==")))))
    (if (multibyte-string-p json) (encode-coding-string json 'utf-8) json)))

(defun ytube--text (node)
  "Return the display text of InnerTube text NODE, or nil."
  (when (hash-table-p node)
    (or (gethash "simpleText" node)
        (let ((runs (gethash "runs" node)))
          (when (vectorp runs)
            (mapconcat (lambda (run) (or (gethash "text" run) "")) runs ""))))))

(defun ytube--video-renderers (node)
  "Return every videoRenderer object inside parsed InnerTube NODE, in order."
  (let (found)
    (cl-labels ((walk (n)
                  (cond ((hash-table-p n)
                         (let ((renderer (gethash "videoRenderer" n)))
                           (if renderer
                               (push renderer found)
                             (maphash (lambda (_key value) (walk value)) n))))
                        ((vectorp n) (mapc #'walk n)))))
      (walk node))
    (nreverse found)))

(defun ytube--parse-innertube (data)
  "Return the videos in DATA, an InnerTube search response parsed as hash tables."
  (let ((seen (make-hash-table :test #'equal))
        videos)
    (dolist (renderer (ytube--video-renderers data))
      (let ((id (gethash "videoId" renderer)))
        (when (and (ytube--valid-id-p id) (not (gethash id seen)))
          (puthash id t seen)
          (push (ytube-video-create
                 :id id
                 :title (or (ytube--text (gethash "title" renderer)) id)
                 :channel (or (ytube--text (gethash "ownerText" renderer))
                              (ytube--text (gethash "longBylineText" renderer)))
                 :duration (or (ytube--text (gethash "lengthText" renderer)) "LIVE")
                 :views (or (ytube--text (gethash "shortViewCountText" renderer))
                            (ytube--text (gethash "viewCountText" renderer)))
                 :published (ytube--text (gethash "publishedTimeText" renderer)))
                videos))))
    (nreverse videos)))

(defun ytube--retrieve-json (url timeout)
  "Fetch URL within TIMEOUT seconds and return its JSON body as hash tables.
Honors the caller's `url-request-method' and related bindings."
  (let ((buffer (url-retrieve-synchronously url t t timeout)))
    (unless buffer
      (error "Request to %s timed out" (url-host (url-generic-parse-url url))))
    (unwind-protect
        (with-current-buffer buffer
          (unless (eql url-http-response-status 200)
            (error "Request to %s failed (HTTP %s)"
                   (url-host (url-generic-parse-url url)) url-http-response-status))
          (json-parse-string
           (decode-coding-string
            (buffer-substring-no-properties (1+ url-http-end-of-headers) (point-max))
            'utf-8)
           :object-type 'hash-table :null-object nil :false-object nil))
      (kill-buffer buffer))))

(defun ytube--search-builtin (query)
  "Search YouTube for QUERY through InnerTube; return a list of videos."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (ytube--innertube-request query)))
    (ytube--parse-innertube
     (ytube--retrieve-json ytube--innertube-url ytube-search-timeout))))

(defun ytube--lookup (id)
  "Return video ID with its title and channel from YouTube's oEmbed endpoint.
Fall back to a video titled by its ID when the lookup fails."
  (or (ignore-errors
        (let ((data (ytube--retrieve-json
                     (concat "https://www.youtube.com/oembed?format=json&url="
                             (url-hexify-string (ytube--watch-url id)))
                     5)))
          (ytube-video-create :id id
                              :title (gethash "title" data)
                              :channel (gethash "author_name" data))))
      (ytube-video-create :id id :title id)))


;;; --- Search: yt-dlp ---------------------------------------------------------

(defun ytube--yt-dlp-args (query)
  "Return yt-dlp arguments listing up to `ytube-max-results' videos for QUERY."
  (list "--ignore-config" "--flat-playlist" "--dump-json" "--no-warnings"
        (format "ytsearch%d:%s" ytube-max-results query)))

(defun ytube--parse-yt-dlp (output)
  "Return the videos in OUTPUT, yt-dlp's one-JSON-object-per-line listing."
  (let (videos)
    (dolist (line (split-string output "\n" t "[ \t\r]+"))
      (when (string-prefix-p "{" line)
        (let* ((entry (json-parse-string line :object-type 'hash-table
                                         :null-object nil :false-object nil))
               (id (gethash "id" entry)))
          (when (ytube--valid-id-p id)
            (push (ytube-video-create
                   :id id
                   :title (or (gethash "title" entry) id)
                   :channel (or (gethash "channel" entry) (gethash "uploader" entry))
                   :duration (or (ytube--format-duration (gethash "duration" entry))
                                 (and (equal (gethash "live_status" entry) "is_live")
                                      "LIVE"))
                   :views (ytube--format-views (gethash "view_count" entry)))
                  videos)))))
    (nreverse videos)))

(defun ytube--search-yt-dlp (query)
  "Search YouTube for QUERY with yt-dlp; return a list of videos."
  (with-temp-buffer
    (let ((status (apply #'call-process ytube-yt-dlp-program nil '(t nil) nil
                         (ytube--yt-dlp-args query))))
      (unless (eql status 0)
        (error "yt-dlp exited with status %s" status))
      (ytube--parse-yt-dlp (buffer-string)))))

(defun ytube--yt-dlp-available-p ()
  "Return non-nil when `ytube-yt-dlp-program' is installed."
  (executable-find ytube-yt-dlp-program))

(defun ytube--search (query)
  "Search YouTube for QUERY with the configured backend; return videos."
  (let ((videos
         (pcase ytube-search-backend
           ('builtin (ytube--search-builtin query))
           ('yt-dlp
            (unless (ytube--yt-dlp-available-p)
              (user-error "yt-dlp not found: %s" ytube-yt-dlp-program))
            (ytube--search-yt-dlp query))
           (_
            (or (and (ytube--yt-dlp-available-p)
                     (condition-case err
                         (ytube--search-yt-dlp query)
                       (error
                        (message "ytube: yt-dlp failed (%s); using builtin search"
                                 (error-message-string err))
                        nil)))
                (ytube--search-builtin query))))))
    (seq-take videos ytube-max-results)))


;;; --- Thumbnails -------------------------------------------------------------

(defvar ytube--thumbnails (make-hash-table :test #'equal)
  "Video ID -> image.  Images start as placeholders and are filled in place.")

(defvar ytube--redisplay-timer nil
  "Pending redisplay after thumbnails arrive.")

(defun ytube--thumbnails-p ()
  "Return non-nil when thumbnails can and should be shown."
  (and ytube-thumbnails
       (display-images-p)
       (image-type-available-p 'jpeg)))

(defun ytube--thumbnail-height ()
  "Return the thumbnail height in pixels."
  (or ytube-thumbnail-height (* 2 (frame-char-height))))

(defun ytube--thumbnail (id)
  "Return the thumbnail image of video ID, fetching it on first use.
The image is a 16:9 placeholder until the download completes; it is
then updated in place, so strings displaying it need no rebuilding."
  (or (gethash id ytube--thumbnails)
      ;; A blank 16x9 bitmap, scaled: empty space of the final size.
      (let ((image (create-image (concat "P1\n16 9\n" (make-string 144 ?0)) 'pbm t
                                 :height (ytube--thumbnail-height)
                                 :ascent 'center)))
        (puthash id image ytube--thumbnails)
        (ytube--fetch-thumbnail id image)
        image)))

(defun ytube--fetch-thumbnail (id image)
  "Download the thumbnail of video ID asynchronously into IMAGE."
  (let ((url (format "https://i.ytimg.com/vi/%s/mqdefault.jpg" id)))
    (condition-case nil
        (url-retrieve
         url
         (lambda (status)
           (let ((buffer (current-buffer)))
             (unwind-protect
                 (when (and (not (plist-get status :error))
                            (eql url-http-response-status 200))
                   (setf (image-property image :data)
                         (buffer-substring-no-properties
                          (1+ url-http-end-of-headers) (point-max)))
                   (setf (image-property image :type) 'jpeg)
                   (ytube--schedule-redisplay))
               (kill-buffer buffer))))
         nil t t)
      ;; Offline or refused: keep the placeholder.
      (error (remhash id ytube--thumbnails)))))

(defun ytube--schedule-redisplay ()
  "Redraw the minibuffer soon, coalescing bursts of thumbnail arrivals."
  (unless (timerp ytube--redisplay-timer)
    (setq ytube--redisplay-timer
          (run-at-time 0.05 nil
                       (lambda ()
                         (setq ytube--redisplay-timer nil)
                         (when-let* ((window (active-minibuffer-window)))
                           (force-window-update window)))))))


;;; --- Completion -------------------------------------------------------------

(defvar ytube-search-history nil
  "Minibuffer history of search queries.")

(defvar ytube--last-results nil
  "Videos from the most recent search, for `ytube-resume'.")

(defconst ytube--channel-width 24
  "Columns reserved for the channel name in result annotations.")

(defun ytube--annotation (video)
  "Return the result annotation for VIDEO: channel, duration, views and age."
  (let ((channel (truncate-string-to-width (or (ytube-video-channel video) "")
                                           ytube--channel-width nil ?\s "…"))
        (meta (string-join (delq nil (list (ytube-video-duration video)
                                           (ytube-video-views video)
                                           (ytube-video-published video)))
                           "  ·  ")))
    (concat "   " (propertize channel 'face 'ytube-channel)
            "  " (propertize meta 'face 'ytube-metadata))))

(defun ytube--candidates (videos)
  "Return an alist of unique candidate strings to VIDEOS, in order."
  (let ((seen (make-hash-table :test #'equal)))
    (mapcar (lambda (video)
              (let ((title (ytube-video-title video)))
                (when (gethash title seen)
                  (setq title (format "%s (%s)" title (ytube-video-id video))))
                (puthash title t seen)
                (cons title video)))
            videos)))

(defun ytube--completion-table (candidates)
  "Return a completion table over CANDIDATES, keeping search order."
  (let ((thumbnails (ytube--thumbnails-p)))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          `(metadata
            (category . ytube-video)
            (display-sort-function . identity)
            (cycle-sort-function . identity)
            (affixation-function
             . ,(lambda (strings)
                  (mapcar
                   (lambda (string)
                     (let ((video (cdr (assoc string candidates))))
                       (list string
                             (if (and thumbnails video)
                                 (concat (propertize " " 'display
                                                     (ytube--thumbnail (ytube-video-id video)))
                                         " ")
                               "")
                             (if video (ytube--annotation video) ""))))
                   strings))))
        (complete-with-action action candidates string predicate)))))

(defun ytube--read-video (videos)
  "Let the user choose one of VIDEOS; return it."
  (let* ((candidates (ytube--candidates videos))
         (choice (completing-read "YouTube: " (ytube--completion-table candidates)
                                  nil t)))
    (cdr (assoc choice candidates))))


;;; --- Local wrapper server ---------------------------------------------------

(defvar ytube--server nil
  "Network process serving player pages on localhost.")

(defvar ytube--titles (make-hash-table :test #'equal)
  "Video ID -> title, for the player page title (and buffer name).")

(defun ytube--html-escape (string)
  "Escape STRING for HTML text and attribute values."
  (replace-regexp-in-string
   "[&<>\"']"
   (lambda (char)
     (pcase char
       ("&" "&amp;") ("<" "&lt;") (">" "&gt;") ("\"" "&quot;") (_ "&#39;")))
   string t t))

(defun ytube--player-html (id)
  "Return the wrapper page embedding video ID through the IFrame API."
  (format "<!doctype html>
<html><head><meta charset=\"utf-8\"><title>%s</title>
<style>html,body{margin:0;height:100%%;background:#000;overflow:hidden}
#player{width:100%%;height:100%%}</style></head>
<body><div id=\"player\"></div><script>
var player;
var api = document.createElement('script');
api.src = 'https://www.youtube.com/iframe_api';
document.head.appendChild(api);
function onYouTubeIframeAPIReady() {
  player = new YT.Player('player', {
    videoId: '%s', host: '%s',
    playerVars: {autoplay: %d, playsinline: 1, rel: 0},
    events: {onReady: function (e) { if (%d) e.target.playVideo(); }}
  });
}
</script></body></html>
"
          (ytube--html-escape (gethash id ytube--titles "YouTube"))
          id
          (ytube--html-escape ytube-embed-host)
          (if ytube-autoplay 1 0)
          (if ytube-autoplay 1 0)))

(defun ytube--server-response (request)
  "Return (STATUS . BODY) answering the HTTP REQUEST text."
  (if (string-match (concat "\\`GET /watch\\?v=\\(" ytube--id-regexp "\\)"
                            "\\(?:&[^ \r\n]*\\)? HTTP/1\\.[01]\r\n")
                    request)
      (cons 200 (ytube--player-html (match-string 1 request)))
    (cons 404 "Not found\n")))

(defun ytube--server-filter (process chunk)
  "Accumulate CHUNK of an HTTP request on PROCESS and answer it once complete."
  (let ((request (concat (process-get process 'ytube-request) chunk)))
    (cond
     ((string-search "\r\n\r\n" request)
      (pcase-let* ((`(,status . ,body) (ytube--server-response request))
                   (body (encode-coding-string body 'utf-8)))
        (process-send-string
         process
         (concat (format (concat "HTTP/1.1 %d %s\r\n"
                                 "Content-Type: text/%s; charset=utf-8\r\n"
                                 "Content-Length: %d\r\n"
                                 "Cache-Control: no-store\r\n"
                                 "Connection: close\r\n\r\n")
                         status (if (= status 200) "OK" "Not Found")
                         (if (= status 200) "html" "plain")
                         (length body))
                 body)))
      (delete-process process))
     ((> (length request) 16384) (delete-process process))
     (t (process-put process 'ytube-request request)))))

(defun ytube--server-port ()
  "Start the wrapper server if needed and return its port."
  (unless (process-live-p ytube--server)
    (setq ytube--server
          (make-network-process :name "ytube-server"
                                :server t
                                :host 'local
                                :family 'ipv4
                                :service t
                                :coding 'binary
                                :noquery t
                                :filter #'ytube--server-filter)))
  (process-contact ytube--server :service))

(defun ytube--player-url (id)
  "Return the localhost URL of the wrapper page for video ID."
  ;; "localhost", not 127.0.0.1: the embed was verified with this origin.
  (format "http://localhost:%d/watch?v=%s" (ytube--server-port) id))

(defun ytube-stop-server ()
  "Stop the localhost wrapper server.  Open players stop working."
  (interactive)
  (when (process-live-p ytube--server)
    (delete-process ytube--server))
  (setq ytube--server nil))


;;; --- Player -----------------------------------------------------------------

(defvar-local ytube--video nil
  "Video shown in this player buffer.")

(defvar ytube-player-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "k") #'ytube-toggle-pause)
    (define-key map (kbd "SPC") #'ytube-toggle-pause)
    (define-key map (kbd "j") #'ytube-seek-backward-10)
    (define-key map (kbd "l") #'ytube-seek-forward-10)
    (define-key map (kbd "<left>") #'ytube-seek-backward-5)
    (define-key map (kbd "<right>") #'ytube-seek-forward-5)
    (define-key map (kbd "m") #'ytube-toggle-mute)
    (define-key map (kbd "<up>") #'ytube-volume-up)
    (define-key map (kbd "<down>") #'ytube-volume-down)
    (define-key map (kbd "<") #'ytube-slower)
    (define-key map (kbd ">") #'ytube-faster)
    (define-key map (kbd "w") #'ytube-copy-url)
    (define-key map (kbd "o") #'ytube-open-externally)
    (define-key map (kbd "s") #'ytube-search)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "Q") #'ytube-kill-player)
    map)
  "Keymap of `ytube-player-mode'.")

(define-minor-mode ytube-player-mode
  "Minor mode of ytube player buffers; see `ytube-player-mode-map'.

\\{ytube-player-mode-map}"
  :lighter " YouTube"
  :keymap ytube-player-mode-map
  (when (and ytube-player-mode (bound-and-true-p evil-local-mode))
    (evil-emacs-state))
  (when ytube-player-mode
    (setq-local header-line-format
                '(:eval (ytube--header-line)))))

(defun ytube--header-line ()
  "Return the player header line."
  (concat
   (if ytube--video
       (concat (propertize (ytube-video-title ytube--video) 'face 'bold)
               (when-let* ((channel (ytube-video-channel ytube--video)))
                 (concat "  " (propertize channel 'face 'ytube-channel))))
     "YouTube")
   (propertize "    k pause  j/l seek  m mute  ↑/↓ volume  </> speed  s search  q quit"
               'face 'ytube-metadata)))

(defun ytube--player-buffers ()
  "Return live player buffers, most recently used first."
  (seq-filter (lambda (buffer) (buffer-local-value 'ytube-player-mode buffer))
              (buffer-list)))

(defun ytube--xwidget (&optional buffer)
  "Return the live xwidget of player BUFFER (default: current), or nil."
  (with-current-buffer (or buffer (current-buffer))
    (when-let* ((xwidget (xwidget-at (point-min))))
      (and (xwidget-live-p xwidget) xwidget))))

(defun ytube--xwidget-callback (xwidget event-type)
  "Handle XWIDGET's EVENT-TYPE like xwidget-webkit, naming the buffer our way."
  (let ((xwidget-webkit-buffer-name-format ytube-player-buffer-name-format))
    (xwidget-webkit-callback xwidget event-type)))

(defun ytube--make-player-buffer ()
  "Create and return an empty player buffer holding a WebKit xwidget."
  (let ((buffer (generate-new-buffer
                 (format-spec ytube-player-buffer-name-format '((?T . ""))))))
    (with-current-buffer buffer
      ;; The xwidget is displayed through a text property on one character.
      (insert " ")
      (let ((xwidget (xwidget-insert (point-min) 'webkit (buffer-name)
                                     (window-body-width nil t)
                                     (window-body-height nil t))))
        (xwidget-put xwidget 'callback #'ytube--xwidget-callback)
        ;; Killing a player needs no confirmation; it is just a video.
        (set-xwidget-query-on-exit-flag xwidget nil))
      (xwidget-webkit-mode)
      (ytube-player-mode 1))
    buffer))

(defun ytube-play-video (video)
  "Play VIDEO, a `ytube-video', in an Emacs player buffer."
  (unless (and (featurep 'xwidget-internal) (display-graphic-p))
    (user-error "ytube needs a graphical Emacs built with xwidgets"))
  (require 'xwidget)
  (let* ((id (ytube-video-id video))
         (buffer (or (and ytube-reuse-player
                          (seq-find #'ytube--xwidget (ytube--player-buffers)))
                     (ytube--make-player-buffer))))
    (puthash id (ytube-video-title video) ytube--titles)
    (with-current-buffer buffer
      (setq ytube--video video)
      (xwidget-webkit-goto-uri (ytube--xwidget) (ytube--player-url id)))
    (pop-to-buffer-same-window buffer)
    (xwidget-webkit-adjust-size-to-window (ytube--xwidget buffer)
                                          (get-buffer-window buffer))))

(defun ytube--player ()
  "Return the xwidget of the current, else most recent, player."
  (or (and ytube-player-mode (ytube--xwidget))
      (seq-some #'ytube--xwidget (ytube--player-buffers))
      (user-error "No YouTube player")))

(defun ytube--player-video ()
  "Return the video of the current, else most recent, player."
  (buffer-local-value 'ytube--video
                      (or (and ytube-player-mode (current-buffer))
                          (car (ytube--player-buffers))
                          (user-error "No YouTube player"))))

(defun ytube--command (script &optional report)
  "Run SCRIPT against `player' in the current player.
When REPORT is non-nil, SCRIPT's string result is shown in the echo area."
  (xwidget-webkit-execute-script
   (ytube--player)
   (format "(function () { if (!window.player || !player.getPlayerState) return null; %s })()"
           script)
   (when report
     (lambda (result)
       (when (and (stringp result) (not (string-empty-p result)))
         (message "YouTube: %s" result))))))

(defun ytube-toggle-pause ()
  "Play or pause the current video."
  (interactive)
  (ytube--command
   "if (player.getPlayerState() === 1) player.pauseVideo(); else player.playVideo();"))

(defun ytube-seek (seconds)
  "Seek SECONDS forward (backward when negative)."
  (interactive "nSeek seconds: ")
  (ytube--command (format "player.seekTo(player.getCurrentTime() + %d, true);"
                          seconds)))

(defun ytube-seek-forward-5 () "Seek 5 seconds forward." (interactive) (ytube-seek 5))
(defun ytube-seek-backward-5 () "Seek 5 seconds back." (interactive) (ytube-seek -5))
(defun ytube-seek-forward-10 () "Seek 10 seconds forward." (interactive) (ytube-seek 10))
(defun ytube-seek-backward-10 () "Seek 10 seconds back." (interactive) (ytube-seek -10))

(defun ytube-toggle-mute ()
  "Mute or unmute the current video."
  (interactive)
  (ytube--command
   "if (player.isMuted()) { player.unMute(); return 'unmuted'; }
    player.mute(); return 'muted';"
   t))

(defun ytube-volume (delta)
  "Change the volume by DELTA percent."
  (interactive "nVolume change: ")
  (ytube--command
   (format "var v = Math.max(0, Math.min(100, player.getVolume() + %d));
            player.unMute(); player.setVolume(v); return 'volume ' + v + '%%';"
           delta)
   t))

(defun ytube-volume-up () "Raise the volume." (interactive) (ytube-volume 10))
(defun ytube-volume-down () "Lower the volume." (interactive) (ytube-volume -10))

(defun ytube-speed (steps)
  "Change the playback rate by STEPS of YouTube's speed list."
  (interactive "nSpeed steps: ")
  (ytube--command
   (format "var r = player.getAvailablePlaybackRates();
            var i = r.indexOf(player.getPlaybackRate()) + %d;
            var v = r[Math.max(0, Math.min(r.length - 1, i))];
            player.setPlaybackRate(v); return 'speed ' + v + 'x';"
           steps)
   t))

(defun ytube-faster () "Play faster." (interactive) (ytube-speed 1))
(defun ytube-slower () "Play slower." (interactive) (ytube-speed -1))

(defun ytube-copy-url ()
  "Copy the current video's youtube.com URL."
  (interactive)
  (let ((url (ytube--watch-url (ytube-video-id (ytube--player-video)))))
    (kill-new url)
    (message "Copied %s" url)))

(defun ytube-open-externally ()
  "Open the current video in the external browser, pausing it here."
  (interactive)
  (let ((video (ytube--player-video)))
    (ytube--command "player.pauseVideo();")
    (browse-url-default-browser (ytube--watch-url (ytube-video-id video)))))

(defun ytube-kill-player ()
  "Kill the current, else most recent, player buffer."
  (interactive)
  (kill-buffer (if ytube-player-mode
                   (current-buffer)
                 (or (car (ytube--player-buffers))
                     (user-error "No YouTube player")))))


;;; --- Commands ---------------------------------------------------------------

(defun ytube--choose-and-play (videos)
  "Prefetch thumbnails of VIDEOS, let the user choose one, and play it."
  (when (ytube--thumbnails-p)
    (dolist (video videos)
      (ytube--thumbnail (ytube-video-id video))))
  (when-let* ((video (ytube--read-video videos)))
    (ytube-play-video video)))

;;;###autoload
(defun ytube-search (query)
  "Search YouTube for QUERY, choose a result and play it inside Emacs."
  (interactive
   (list (read-string "Search YouTube: " nil 'ytube-search-history)))
  (when (string-blank-p query)
    (user-error "Empty search"))
  (let ((videos (with-temp-message (format "Searching YouTube for %s…" query)
                  (ytube--search query))))
    (unless videos
      (user-error "No YouTube results for %s" query))
    (setq ytube--last-results videos)
    (ytube--choose-and-play videos)))

;;;###autoload
(defun ytube-resume ()
  "Choose again from the most recent search results."
  (interactive)
  (unless ytube--last-results
    (user-error "No previous YouTube search"))
  (ytube--choose-and-play ytube--last-results))

;;;###autoload
(defun ytube-play (url-or-id)
  "Play the YouTube video at URL-OR-ID inside Emacs."
  (interactive
   (list (read-string "YouTube URL or ID: "
                      (when-let* ((url (thing-at-point 'url t)))
                        (and (ytube--extract-id url) url)))))
  (let ((id (or (ytube--extract-id url-or-id)
                (user-error "Not a YouTube video: %s" url-or-id))))
    (ytube-play-video (with-temp-message "Looking up video…" (ytube--lookup id)))))

;;;###autoload
(defun ytube-toggle-thumbnails ()
  "Toggle thumbnails in search results."
  (interactive)
  (setq ytube-thumbnails (not ytube-thumbnails))
  (message "ytube thumbnails %s" (if ytube-thumbnails "on" "off")))

(provide 'ytube)
;;; ytube.el ends here
