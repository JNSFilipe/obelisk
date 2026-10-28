;;; ytube-test.el --- ytube regression scenarios -*- lexical-binding: t; -*-
(require 'ert)
(load (expand-file-name "../ytube.el" (file-name-directory load-file-name)) nil t)

(defconst ytube-test-innertube
  "{\"contents\":{\"sectionListRenderer\":{\"contents\":[{\"itemSectionRenderer\":{\"contents\":[
 {\"videoRenderer\":{\"videoId\":\"aaaaaaaaaaa\",
   \"title\":{\"runs\":[{\"text\":\"First \"},{\"text\":\"video\"}]},
   \"ownerText\":{\"runs\":[{\"text\":\"Channel One\"}]},
   \"lengthText\":{\"simpleText\":\"4:53\"},
   \"shortViewCountText\":{\"simpleText\":\"97K views\"},
   \"viewCountText\":{\"simpleText\":\"96,994 views\"},
   \"publishedTimeText\":{\"simpleText\":\"5 years ago\"}}},
 {\"adSlotRenderer\":{}},
 {\"shelfRenderer\":{\"content\":{\"items\":[
   {\"videoRenderer\":{\"videoId\":\"bbbbbbbbbbb\",
     \"title\":{\"simpleText\":\"Live — ünïcode\"},
     \"longBylineText\":{\"runs\":[{\"text\":\"Channel Two\"}]},
     \"viewCountText\":{\"runs\":[{\"text\":\"1,234\"},{\"text\":\" watching\"}]}}},
   {\"videoRenderer\":{\"videoId\":\"aaaaaaaaaaa\",\"title\":{\"simpleText\":\"Duplicate\"}}},
   {\"videoRenderer\":{\"videoId\":\"bad\"}}]}}}]}}]}}}"
  "Trimmed InnerTube search response: nesting, a shelf, a duplicate, a bad ID.")

(defconst ytube-test-yt-dlp
  (concat
   "{\"id\": \"ccccccccccc\", \"title\": \"Org basics\", \"channel\": \"Crafters\","
   " \"uploader\": \"Crafters\", \"duration\": 2857, \"view_count\": 91431,"
   " \"live_status\": \"was_live\"}\n"
   "WARNING: not json\n"
   "{\"id\": \"ddddddddddd\", \"title\": \"Streaming now\", \"channel\": null,"
   " \"uploader\": \"Uploader\", \"duration\": null, \"view_count\": 7,"
   " \"live_status\": \"is_live\"}\n")
  "yt-dlp --flat-playlist --dump-json output, in the shape of a real run.")

(defun ytube-test-fake-program (output status)
  "Return a fake yt-dlp script printing OUTPUT and exiting with STATUS."
  (let ((file (make-temp-file "ytube-fake-yt-dlp-")))
    (with-temp-file file
      (insert "#!/bin/sh\ncat <<'EOF'\n" output "EOF\nexit " (number-to-string status) "\n"))
    (set-file-modes file #o700)
    file))


;;; --- Parsing ----------------------------------------------------------------

(ert-deftest ytube-parses-innertube-results ()
  (let ((videos (ytube--parse-innertube
                 (json-parse-string ytube-test-innertube :object-type 'hash-table
                                    :null-object nil :false-object nil))))
    (should (equal (mapcar #'ytube-video-id videos) '("aaaaaaaaaaa" "bbbbbbbbbbb")))
    (let ((first (car videos)) (live (cadr videos)))
      (should (equal (ytube-video-title first) "First video"))
      (should (equal (ytube-video-channel first) "Channel One"))
      (should (equal (ytube-video-duration first) "4:53"))
      (should (equal (ytube-video-views first) "97K views"))
      (should (equal (ytube-video-published first) "5 years ago"))
      (should (equal (ytube-video-title live) "Live — ünïcode"))
      (should (equal (ytube-video-channel live) "Channel Two"))
      (should (equal (ytube-video-duration live) "LIVE"))
      (should (equal (ytube-video-views live) "1,234 watching"))
      (should-not (ytube-video-published live)))))

(ert-deftest ytube-parses-yt-dlp-output ()
  (let ((videos (ytube--parse-yt-dlp ytube-test-yt-dlp)))
    (should (equal (mapcar #'ytube-video-id videos) '("ccccccccccc" "ddddddddddd")))
    (should (equal (ytube-video-duration (car videos)) "47:37"))
    (should (equal (ytube-video-views (car videos)) "91K views"))
    (should (equal (ytube-video-channel (cadr videos)) "Uploader"))
    (should (equal (ytube-video-duration (cadr videos)) "LIVE"))
    (should (equal (ytube-video-views (cadr videos)) "7 views"))))

(ert-deftest ytube-innertube-request-is-utf8-json ()
  (let* ((body (ytube--innertube-request "café \"org\""))
         (data (json-parse-string (decode-coding-string body 'utf-8))))
    (should-not (multibyte-string-p body))
    (should (equal (gethash "query" data) "café \"org\""))
    (should (equal (gethash "clientName" (gethash "client" (gethash "context" data)))
                   "WEB"))))

(ert-deftest ytube-formats-durations-and-views ()
  (should (equal (ytube--format-duration 59) "0:59"))
  (should (equal (ytube--format-duration 3725.9) "1:02:05"))
  (should-not (ytube--format-duration nil))
  (should (equal (ytube--format-views 1) "1 view"))
  (should (equal (ytube--format-views 999) "999 views"))
  (should (equal (ytube--format-views 1000) "1K views"))
  (should (equal (ytube--format-views 1290) "1.2K views"))
  (should (equal (ytube--format-views 96994) "96K views"))
  (should (equal (ytube--format-views 2500000000) "2.5B views"))
  (should-not (ytube--format-views nil)))

(ert-deftest ytube-extracts-video-ids ()
  (dolist (input '("dQw4w9WgXcQ"
                   " https://www.youtube.com/watch?v=dQw4w9WgXcQ "
                   "https://www.youtube.com/watch?feature=share&v=dQw4w9WgXcQ&t=42"
                   "https://youtu.be/dQw4w9WgXcQ?si=abc"
                   "https://www.youtube.com/shorts/dQw4w9WgXcQ"
                   "https://www.youtube-nocookie.com/embed/dQw4w9WgXcQ"
                   "https://www.youtube.com/live/dQw4w9WgXcQ"))
    (should (equal (ytube--extract-id input) "dQw4w9WgXcQ")))
  (should-not (ytube--extract-id "https://example.com/"))
  (should-not (ytube--extract-id "https://youtu.be/dQw4w9WgXcQextra")))


;;; --- Backend selection ------------------------------------------------------

(ert-deftest ytube-auto-uses-builtin-without-yt-dlp ()
  (let ((ytube-search-backend 'auto)
        (ytube-yt-dlp-program "ytube-no-such-program"))
    (cl-letf (((symbol-function 'ytube--search-builtin)
               (lambda (query) (list (ytube-video-create :id "eeeeeeeeeee" :title query)))))
      (should (equal (mapcar #'ytube-video-title (ytube--search "q")) '("q"))))))

(ert-deftest ytube-auto-prefers-yt-dlp ()
  (let* ((ytube-search-backend 'auto)
         (ytube-yt-dlp-program (ytube-test-fake-program ytube-test-yt-dlp 0)))
    (unwind-protect
        (cl-letf (((symbol-function 'ytube--search-builtin)
                   (lambda (_) (error "Builtin must not run"))))
          (should (equal (mapcar #'ytube-video-id (ytube--search "q"))
                         '("ccccccccccc" "ddddddddddd"))))
      (delete-file ytube-yt-dlp-program))))

(ert-deftest ytube-auto-falls-back-when-yt-dlp-fails ()
  (let* ((ytube-search-backend 'auto)
         (ytube-yt-dlp-program (ytube-test-fake-program "" 1)))
    (unwind-protect
        (cl-letf (((symbol-function 'ytube--search-builtin)
                   (lambda (_) (list (ytube-video-create :id "eeeeeeeeeee" :title "b")))))
          (should (equal (mapcar #'ytube-video-id (ytube--search "q")) '("eeeeeeeeeee"))))
      (delete-file ytube-yt-dlp-program))))

(ert-deftest ytube-explicit-yt-dlp-reports-missing-program ()
  (let ((ytube-search-backend 'yt-dlp)
        (ytube-yt-dlp-program "ytube-no-such-program"))
    (should-error (ytube--search "q") :type 'user-error)))

(ert-deftest ytube-yt-dlp-args-honor-max-results ()
  (let ((ytube-max-results 7))
    (should (equal (car (last (ytube--yt-dlp-args "a b"))) "ytsearch7:a b"))
    (should (member "--ignore-config" (ytube--yt-dlp-args "a")))))

(ert-deftest ytube-search-truncates-results ()
  (let ((ytube-search-backend 'builtin)
        (ytube-max-results 1))
    (cl-letf (((symbol-function 'ytube--search-builtin)
               (lambda (_) (ytube--parse-yt-dlp ytube-test-yt-dlp))))
      (should (= 1 (length (ytube--search "q")))))))


;;; --- Completion -------------------------------------------------------------

(ert-deftest ytube-completion-keeps-order-and-annotates ()
  (let* ((ytube-thumbnails nil)
         (videos (ytube--parse-yt-dlp ytube-test-yt-dlp))
         (candidates (ytube--candidates videos))
         (table (ytube--completion-table candidates))
         (metadata (cdr (funcall table "" nil 'metadata)))
         (affix (funcall (alist-get 'affixation-function metadata)
                         '("Org basics"))))
    (should (eq (alist-get 'category metadata) 'ytube-video))
    (should (eq (alist-get 'display-sort-function metadata) #'identity))
    (should (equal (all-completions "" table) '("Org basics" "Streaming now")))
    (pcase-let ((`((,cand ,prefix ,suffix)) affix))
      (should (equal cand "Org basics"))
      (should (equal prefix ""))
      (should (string-match-p "Crafters +47:37  ·  91K views" suffix)))))

(ert-deftest ytube-candidates-are-unique ()
  (let ((candidates (ytube--candidates
                     (list (ytube-video-create :id "aaaaaaaaaaa" :title "Same")
                           (ytube-video-create :id "bbbbbbbbbbb" :title "Same")))))
    (should (equal (mapcar #'car candidates) '("Same" "Same (bbbbbbbbbbb)")))))

(ert-deftest ytube-thumbnails-off-without-graphics ()
  ;; Batch Emacs cannot display images; thumbnails must not be attempted.
  (let ((ytube-thumbnails t))
    (should-not (ytube--thumbnails-p))))


;;; --- Wrapper server ---------------------------------------------------------

(ert-deftest ytube-player-html-escapes-title ()
  (let ((ytube--titles (make-hash-table :test #'equal)))
    (puthash "aaaaaaaaaaa" "<script>&\"'" ytube--titles)
    (let ((html (ytube--player-html "aaaaaaaaaaa")))
      (should (string-search "<title>&lt;script&gt;&amp;&quot;&#39;</title>" html))
      (should (string-search "videoId: 'aaaaaaaaaaa'" html))
      (should (string-search "height:100%;" html)))))

(ert-deftest ytube-server-rejects-other-requests ()
  (should (= 404 (car (ytube--server-response "GET /favicon.ico HTTP/1.1\r\n\r\n"))))
  (should (= 404 (car (ytube--server-response "POST /watch?v=aaaaaaaaaaa HTTP/1.1\r\n\r\n"))))
  (should (= 404 (car (ytube--server-response "GET /watch?v=aaaaaaaaaaa' HTTP/1.1\r\n\r\n"))))
  (should (= 200 (car (ytube--server-response
                       "GET /watch?v=aaaaaaaaaaa&x=1 HTTP/1.1\r\n\r\n")))))

(ert-deftest ytube-server-serves-over-http ()
  (let ((ytube--titles (make-hash-table :test #'equal))
        (ytube--server nil))
    (puthash "aaaaaaaaaaa" "Café" ytube--titles)
    (unwind-protect
        (let ((port (ytube--server-port)))
          (should (string-prefix-p (format "http://localhost:%d/watch?v=" port)
                                   (ytube--player-url "aaaaaaaaaaa")))
          (with-current-buffer (url-retrieve-synchronously
                                (format "http://127.0.0.1:%d/watch?v=aaaaaaaaaaa" port)
                                t t 5)
            (should (eql url-http-response-status 200))
            (should (string-search
                     "<title>Café</title>"
                     (decode-coding-string
                      (buffer-substring (1+ url-http-end-of-headers) (point-max)) 'utf-8)))
            (kill-buffer))
          (with-current-buffer (url-retrieve-synchronously
                                (format "http://127.0.0.1:%d/other" port) t t 5)
            (should (eql url-http-response-status 404))
            (kill-buffer))
          ;; The same server keeps serving.
          (should (= port (ytube--server-port))))
      (ytube-stop-server))
    (should-not ytube--server)))

;;; ytube-test.el ends here
