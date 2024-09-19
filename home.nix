{ config, pkgs, ... }:

let
  cfg = {
    user = "jfilipe";
    home = "/home/${cfg.user}";
    dots = "${cfg.home}/Documents/GitHub/obelisk";
  };
in
{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = cfg.user;
  home.homeDirectory = cfg.home;

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # # # Adds the 'hello' command to your environment. It prints a friendly
    # # # "Hello, world!" when run.
    # # pkgs.hello

    # # Terminal Utils
    # pkgs.gh
    # pkgs.zsh
    # pkgs.fzf
    # pkgs.eza
    # pkgs.htop
    # pkgs.tmux
    # pkgs.zoxide
    # pkgs.lazygit
    # pkgs.ripgrep
    # pkgs.starship
    # pkgs.editorconfig-core-c

    # # LSPs / Compilers / Interpreters / Etc
    # pkgs.tree-sitter
    # pkgs.clang-tools
    # pkgs.nodejs
    # pkgs.sbcl
    # pkgs.gopls
    # pkgs.gomodifytags
    # pkgs.gotests
    # pkgs.gore
    # pkgs.rustup
    # pkgs.shellcheck
    # pkgs.shfmt
    # pkgs.zig
    # pkgs.texliveFull
    # pkgs.pipenv
    # (pkgs.python312.withPackages (ppkgs: [
    #   ppkgs.nose
    #   ppkgs.isort
    #   ppkgs.black
    #   ppkgs.pytest
    #   ppkgs.pyflakes
    # ]))

    # # Editors
    # pkgs.emacs
    # pkgs.neovim

    # # Apps
    # pkgs.github-desktop
    # pkgs.simulide

    # # Emuatores
    # # pkgs.wine-wayland
    # # pkgs.wine-staging
    # pkgs.wineWowPackages.waylandFull
    # pkgs.winetricks

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })
    (pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

   nixpkgs = {
    config = {
      allowUnfree = true;
      allowUnfreePredicate = (_: true);
    };
  };

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  # https://seroperson.me/2024/01/16/managing-dotfiles-with-nix/

  # Configs in HOME
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;
    ".tmux.conf".source = config.lib.file.mkOutOfStoreSymlink "${cfg.dots}/tmux/tmux.conf";
    ".bashrc".source = config.lib.file.mkOutOfStoreSymlink "${cfg.dots}/bash/bashrc";
    ".zshrc".source = config.lib.file.mkOutOfStoreSymlink "${cfg.dots}/zsh/zshrc";

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Configs in XDG_CONFIG
  xdg.configFile = {
    # Emacs
    "doom" = {
      source = config.lib.file.mkOutOfStoreSymlink "${cfg.dots}/emacs";
      recursive = true;
    };

    # NeoVim
    "nvim" = {
      source = config.lib.file.mkOutOfStoreSymlink "${cfg.dots}/neovim";
      recursive = true;
    };
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/jfilipe/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    EDITOR = "emacs";
  };

  # Build font cache
  fonts.fontconfig.enable = true;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
