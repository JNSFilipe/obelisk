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

{
  programs.nushell = {
    enable = true;
    package = pkgs.nushell;

    # home.sessionVariables land in a POSIX profile script that Nushell cannot
    # source, so the ones that matter are restated here.
    environmentVariables = {
      EDITOR = "emacsclient -t";
      VISUAL = "emacsclient -c";
      _ZO_DOCTOR = "0";
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

      # Ordered after the config above, matching home-manager's own atuin wiring.
      (lib.mkOrder 2000 ''
        source ${
          pkgs.runCommand "atuin-nushell-config.nu"
            {
              nativeBuildInputs = [ pkgs.writableTmpDirAsHomeHook ];
            }
            ''
              ${lib.getExe config.programs.atuin.package} init nu ${lib.escapeShellArgs config.programs.atuin.flags} \
                | awk '/name: atuin$/ { n++; if (n > 1) sub(/name: atuin$/, "name: atuin_" n) } 1' >> "$out"
            ''
        }
      '')
    ];
  };

  # ── Integrations ────────────────────────────────────────────────────────────
  # Each of these already has a Zsh integration switched on elsewhere; only the
  # Nushell half is added here.  Nothing below changes the Zsh setup.

  # Atuin's own Nushell init names both of its keybindings `atuin`, which makes
  # Nushell print a ~20-line "Multiple keybindings share a name" warning at every
  # startup.  Both bindings work, so this is cosmetic, but not at every prompt.
  # Generate the same file home-manager would and give the second one a distinct
  # name.  If upstream renames them, the awk simply matches nothing.
  programs.atuin.enableNushellIntegration = false;
  programs.zoxide.enableNushellIntegration = true;
  programs.yazi.enableNushellIntegration = true;

  # Carapace replaces fzf-tab's role: argument completion for external commands.
  # CARAPACE_BRIDGES falls back to Zsh's completion definitions for anything
  # carapace has no spec for, so the long tail is still covered.
  programs.carapace = {
    enable = true;
    enableNushellIntegration = true;
    # Zsh is deliberately left exactly as it was.
    enableZshIntegration = false;
    enableBashIntegration = false;
    enableFishIntegration = false;
    environment.CARAPACE_BRIDGES = "zsh";
    extraPackages = [ pkgs.zsh ];
  };

  # fzf keeps its Zsh integration and gains no Nushell one, as requested.
  programs.fzf.enableNushellIntegration = false;
}
