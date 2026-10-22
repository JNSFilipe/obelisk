#!/usr/bin/env python3
"""Check generated Pi activation for all hosts with isolated state and a fake Pi.

Run after `nix flake check` has built the manifest and activation script inputs.
"""

import json
import os
import shlex
import subprocess
import sys
import tempfile
from pathlib import Path

REPO = Path(__file__).resolve().parents[1]
PACKAGES = [
    "npm:pi-web-access",
    "npm:@juicesharp/rpiv-ask-user-question",
    "npm:@heyhuynhgiabuu/pi-task",
    "git:github.com/JNSFilipe/jig",
]


def main():
    for host in ("gauss", "thorp", "gauss-locked", "thorp-locked"):
        prefix = f"{REPO}#darwinConfigurations.{host}.config.home-manager.users.jfilipe"
        activation = subprocess.check_output(
            ["nix", "eval", "--raw", f"{prefix}.home.activation.piPackages.data"],
            text=True,
            timeout=90,
        )
        home_directory = json.loads(
            subprocess.check_output(
                ["nix", "eval", "--json", f"{prefix}.home.homeDirectory"],
                text=True,
                timeout=90,
            )
        )
        updates = not host.endswith("-locked")
        assert (
            "/opt/homebrew/bin/pi update --extensions --no-approve" in activation
        ) == updates
        assert "run /opt/homebrew/bin/python3.13" in activation
        with tempfile.TemporaryDirectory(prefix="pi-activation-test-") as tmp:
            home = (Path(tmp) / "home").resolve()
            agent = home / ".pi/agent"
            agent.mkdir(parents=True)
            jig = agent / "git/github.com/JNSFilipe/jig/skills/jig-apply"
            if not updates:
                jig.mkdir(parents=True)
                (jig / "SKILL.md").write_text("---\nname: jig-apply\n---\nSkill\n")
            settings = agent / "settings.json"
            original = {"packages": ["npm:old"], "deviceId": "local", "theme": "custom"}
            settings.write_text(json.dumps(original))
            before = settings.read_bytes()
            log = Path(tmp) / "pi.log"
            fake_pi = Path(tmp) / "fake-pi"
            fake_pi.write_text(f"#!{sys.executable}\n" + """
import json, os, sys
from pathlib import Path
Path(os.environ['PI_ACTIVATION_LOG']).write_text(json.dumps({
    'args': sys.argv[1:], 'cwd': os.getcwd(),
    'agent': os.environ.get('PI_CODING_AGENT_DIR'),
}))
status = int(os.environ.get('PI_ACTIVATION_EXIT', '0'))
if status == 0:
    # Simulate a fresh Git installation: sharing must happen AFTER Pi update.
    skill = Path(os.environ['PI_CODING_AGENT_DIR']) / 'git/github.com/JNSFilipe/jig/skills/jig-apply'
    skill.mkdir(parents=True, exist_ok=True)
    (skill / 'SKILL.md').write_text('Jig skill')
sys.exit(status)
""")
            fake_pi.chmod(0o700)
            # Never execute the live Pi, even if the activation bypasses run.
            script = activation.replace(home_directory, str(home))
            script = script.replace(
                "/opt/homebrew/bin/python3.13", shlex.quote(sys.executable)
            )
            script = script.replace("/opt/homebrew/bin/pi", shlex.quote(str(fake_pi)))
            # Match Home Manager's run contract; set -e catches failed commands.
            script = (
                'set -e\nrun() { if [[ -n ${DRY_RUN+x} ]]; then return 0; fi; "$@"; }\n'
                + script
            )
            env = dict(
                os.environ,
                HOME=str(home),
                PI_ACTIVATION_LOG=str(log),
                PI_CODING_AGENT_DIR="/must-not-use-inherited-directory",
            )
            env.pop("DRY_RUN", None)
            subprocess.run(
                ["bash", "-c", script],
                env=dict(env, DRY_RUN="1"),
                check=True,
                timeout=30,
            )
            assert settings.read_bytes() == before and not log.exists(), host
            assert not (home / ".claude").exists(), host
            assert not (home / ".codex").exists(), host
            subprocess.run(["bash", "-c", script], env=env, check=True, timeout=30)
            assert json.loads(settings.read_text()) == original | {
                "packages": PACKAGES
            }, host
            for tool in (".claude", ".codex"):
                link = home / tool / "skills/jig-apply"
                assert link.is_symlink() and link.resolve() == jig, (host, link)
            # Sharing must converge without replacing its existing links.
            subprocess.run(["bash", "-c", script], env=env, check=True, timeout=30)
            if updates:
                observed = json.loads(log.read_text())
                assert observed == {
                    "args": ["update", "--extensions", "--no-approve"],
                    "cwd": str(home),
                    "agent": str(agent),
                }, (host, observed)
                failure = subprocess.run(
                    ["bash", "-c", script],
                    env=dict(env, PI_ACTIVATION_EXIT="7"),
                    check=False,
                    timeout=30,
                )
                assert failure.returncode == 7, (host, failure.returncode)
            else:
                assert not log.exists(), host
        print(
            f"PASS {host}: writable merge, dry run, idempotent skill sharing, "
            + ("global update/error propagation" if updates else "no updates")
        )


if __name__ == "__main__":
    main()
