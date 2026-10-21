{
  config,
  lib,
  pkgs,
  ...
}:

# Nushell, installed alongside Zsh rather than replacing it.  Zsh stays the
# login shell (see nix/darwin.nix); this is started explicitly with `nu`.
#
# Everything Zsh gets from plugins, Nushell has built in: history hints,
# syntax highlighting, fuzzy completion and a configurable prompt.  The only
# external completion helper is carapace, which also bridges to Zsh's own
# completion definitions for commands it does not know.
#
# The interactive half of the configuration is real Nushell code and lives in
# configs/nushell/config.nu.

let
  integrationDir = "${config.xdg.cacheHome}/obelisk/nushell";
  # Nushell's source paths are resolved at parse time. Its env.nu is evaluated
  # before config.nu is parsed, so regenerate these with the installed Homebrew
  # binaries there, not during a sandboxed Nix build. This also tracks upgrades.
  refreshIntegrations = pkgs.writeText "refresh-nushell-integrations.sh" ''
    set -eu
    export PATH=/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin
    cache=$1
    mkdir -p -- "$cache"
    tmp=$(mktemp -d "$cache/.init.XXXXXX")
    trap 'rm -rf -- "$tmp"' EXIT HUP INT TERM
    /opt/homebrew/bin/zoxide init nushell --cmd cd > "$tmp/zoxide.nu"
    /opt/homebrew/bin/carapace _carapace nushell > "$tmp/carapace.nu"
    /opt/homebrew/bin/atuin init nu > "$tmp/atuin-raw.nu"
    # Older Atuin releases give both keybindings the same name. This is a
    # no-op on releases that already give them distinct names.
    /usr/bin/awk '/name: atuin$/ { n++; if (n > 1) sub(/name: atuin$/, "name: atuin_" n) } 1' \
      "$tmp/atuin-raw.nu" > "$tmp/atuin.nu"
    for name in zoxide carapace atuin; do
      mv -f -- "$tmp/$name.nu" "$cache/$name.nu"
    done
  '';
in
{
  programs.nushell = {
    enable = true;
    package = null; # Homebrew owns the runtime; keep declarative config here.

    extraEnv = ''
      ^/bin/sh ${refreshIntegrations} ${lib.hm.nushell.toNushell { } integrationDir}
    '';

    # home.sessionVariables land in a POSIX profile script that Nushell cannot
    # source, so the ones that matter are restated here.
    environmentVariables = {
      EDITOR = "emacsclient -t";
      VISUAL = "emacsclient -c";
      _ZO_DOCTOR = "0";
      CARAPACE_BRIDGES = "zsh";
    };

    settings = {
      show_banner = false;
      edit_mode = "emacs";

      history = {
        max_size = 10000000;
        file_format = "sqlite";
        # Matches zsh's `share = true`: one history across concurrent sessions.
        isolation = false;
      };

      completions = {
        # The reason carapace is enough here and fzf-tab is not needed.
        algorithm = "fuzzy";
        case_sensitive = false;
        quick = true;
        partial = true;
      };
    };

    # Only the aliases that survive the move verbatim.  The zsh `ls` family
    # relies on `gls` and on zsh glob qualifiers (`*(/N)`), so it is rewritten
    # as Nushell commands in config.nu instead, keeping the builtin `ls` table.
    shellAliases = {
      vi = "nvim";
      vim = "nvim";
      ec = "emacsclient -c";
      et = "emacsclient -t";
      ww = "wget";
      del = "sudo rm -r";
      comp = "sudo make install";
      gl = "git clone";
      lg = "lazygit";
      rcp = "rsync -avzh --progress --stats";
      brewdeps = "brew deps --installed --tree";
      brewup = "brew upgrade --greedy";
      caskup = "brew upgrade --cask --greedy";
      brewrm-msupdate = "brew uninstall microsoft-auto-update";
    };

    extraConfig = lib.mkMerge [
      (builtins.readFile ../configs/nushell/config.nu)

      (lib.mkOrder 2000 ''
        source ${lib.hm.nushell.toNushell { } "${integrationDir}/zoxide.nu"}
        source ${lib.hm.nushell.toNushell { } "${integrationDir}/carapace.nu"}
        source ${lib.hm.nushell.toNushell { } "${integrationDir}/atuin.nu"}
        if ("TMUX" in $env) and ("ATUIN_SESSION" in $env) {
          do --ignore-errors { ^/opt/homebrew/bin/tmux setenv ATUIN_SESSION $env.ATUIN_SESSION }
        }
      '')
    ];
  };

  # Yazi's wrapper calls the command on PATH and supports package = null.
  # Atuin, zoxide, fzf, opam and carapace's modules require Nix packages, so
  # those modules stay disabled; their Homebrew integrations are wired above.
  programs.yazi.enableNushellIntegration = true;
}
