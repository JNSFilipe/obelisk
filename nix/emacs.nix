{
  config,
  homeDirectory,
  lib,
  pkgs,
  userName,
  ...
}:
let
  doomRoot = "${homeDirectory}/.local/share/doom-emacs";
  emacs = "/opt/homebrew/bin/emacs";
  environment = ''
    export EMACS=${emacs}
    export DOOMDIR=${homeDirectory}/.config/doom
    export DOOMLOCALDIR=${doomRoot}/.local
  '';
in
{
  # Doom must remain writable for straight, sync and upgrade. Home Manager
  # backs up the previous directory using backupFileExtension on migration.
  home.file.".config/emacs".source = config.lib.file.mkOutOfStoreSymlink doomRoot;

  home.packages = [
    (pkgs.writeShellScriptBin "emacs" ''
      ${environment}
      exec ${emacs} --init-directory=${doomRoot} "$@"
    '')
    (pkgs.writeShellScriptBin "emacsclient" ''
      exec /opt/homebrew/bin/emacsclient "$@"
    '')
    (pkgs.writeShellScriptBin "doom" ''
      ${environment}
      if [ ! -x ${doomRoot}/bin/doom ]; then
        echo "Doom is not installed; run make doom-install in obelisk." >&2
        exit 1
      fi
      exec ${doomRoot}/bin/doom "$@"
    '')
    (pkgs.writeShellScriptBin "vanilla-emacs" ''
      exec ${emacs} --init-directory=${homeDirectory}/.config/vemacs "$@"
    '')
  ];

  # Homebrew supplies the app, but launchd has exactly one service owner.
  # Retain Home Manager's existing label so activation replaces that service.
  launchd.agents.emacs = {
    enable = true;
    config = {
      ProgramArguments = [
        emacs
        "--init-directory=${doomRoot}"
        "--fg-daemon"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      ThrottleInterval = 30;
      StandardOutPath = "${homeDirectory}/Library/Logs/emacs.log";
      StandardErrorPath = "${homeDirectory}/Library/Logs/emacs.error.log";
      EnvironmentVariables = {
        HOME = homeDirectory;
        DOOMDIR = "${homeDirectory}/.config/doom";
        DOOMLOCALDIR = "${doomRoot}/.local";
        PATH = lib.concatStringsSep ":" [
          "${homeDirectory}/.local/bin"
          "${homeDirectory}/.cargo/bin"
          "${homeDirectory}/.config/scripts"
          "/etc/profiles/per-user/${userName}/bin"
          "/run/current-system/sw/bin"
          "/nix/var/nix/profiles/default/bin"
          "/opt/homebrew/bin"
          "/opt/homebrew/sbin"
          "/opt/homebrew/opt/llvm/bin"
          "/opt/homebrew/opt/rustup/bin"
          "/opt/homebrew/opt/postgresql@18/bin"
          "/Library/TeX/texbin"
          "/usr/bin"
          "/bin"
          "/usr/sbin"
          "/sbin"
        ];
      };
    };
  };
}
