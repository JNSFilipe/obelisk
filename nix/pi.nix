{
  config,
  darwinConfig,
  lib,
  pkgs,
  ...
}:

let
  # Shared on every host. Versions intentionally float independently of flake.lock.
  packages = [
    "npm:pi-web-access"
    "npm:@juicesharp/rpiv-ask-user-question"
    "npm:@heyhuynhgiabuu/pi-task"
    "git:github.com/JNSFilipe/jig"
  ];
  manifest = pkgs.writeText "pi-packages.json" (builtins.toJSON packages);
  agentDirectory = "${config.home.homeDirectory}/.pi/agent";
in
{
  # Do not use home.file: Pi writes settings itself. Merge only the authoritative
  # package list, keeping preferences/device identity local and settings writable.
  # nix-darwin runs Homebrew before Home Manager, so both executables exist here.
  # Follow Homebrew's upgrade policy: locked configurations only sync settings.
  home.activation.piPackages = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    run /opt/homebrew/bin/python3.13 ${../configs/scripts/pi-sync-packages.py} \
      ${manifest} ${lib.escapeShellArg "${agentDirectory}/settings.json"}
    ${lib.optionalString darwinConfig.homebrew.onActivation.upgrade ''
      (
        cd ${lib.escapeShellArg config.home.homeDirectory}
        run /usr/bin/env ${lib.escapeShellArg "PI_CODING_AGENT_DIR=${agentDirectory}"} \
          PI_SKIP_VERSION_CHECK=1 /opt/homebrew/bin/pi update --extensions --no-approve
      )
    ''}
    # Pi owns the only installed Jig copy. Other clients consume symlinks, not
    # copied skills. A fresh locked activation defers sharing until Pi installs it.
    run /opt/homebrew/bin/python3.13 ${../configs/scripts/pi-share-jig.py} \
      ${lib.escapeShellArg "${agentDirectory}/git/github.com/JNSFilipe/jig/skills"} \
      ${lib.escapeShellArg config.home.homeDirectory}
  '';
}
