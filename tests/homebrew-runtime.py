#!/usr/bin/env python3
"""Smoke-test generated configs with Homebrew runtimes, without touching user state.

Run after `make build`: python3 tests/homebrew-runtime.py --host thorp
"""

import argparse
import errno
import fcntl
import json
import os
import pty
import select
import signal
import sqlite3
import struct
import subprocess
import tempfile
import termios
import time
from contextlib import closing
from pathlib import Path

REPO = Path(__file__).resolve().parents[1]
BREW = Path("/opt/homebrew/bin")


def run(args, **kwargs):
    result = subprocess.run(
        args,
        text=True,
        capture_output=True,
        check=False,
        timeout=90,
        **kwargs,
    )
    if result.returncode:
        raise RuntimeError(f"{args[0]} exited {result.returncode}:\n{result.stderr}")
    return result


def run_terminal(args, env):
    """ZLE and autocomplete need a controlling terminal, not captured pipes."""
    pid, master = pty.fork()
    if pid == 0:
        os.execve(args[0], args, env)
    fcntl.ioctl(master, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 120, 0, 0))
    output = bytearray()
    deadline = time.monotonic() + 90
    try:
        while time.monotonic() < deadline:
            if select.select([master], [], [], 0.1)[0]:
                try:
                    chunk = os.read(master, 65536)
                except OSError as error:
                    if error.errno == errno.EIO:
                        break
                    raise
                if not chunk:
                    break
                output.extend(chunk)
        else:
            os.kill(pid, signal.SIGKILL)
            raise TimeoutError("Zsh runtime test timed out")
    finally:
        os.close(master)
        _, status = os.waitpid(pid, 0)
    text = output.decode(errors="replace").replace("\r", "")
    if os.waitstatus_to_exitcode(status):
        raise RuntimeError(f"Zsh test failed ({status}):\n{text}")
    return text


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--host", required=True, choices=("gauss", "thorp"))
    args = parser.parse_args()
    prefix = (
        f"{REPO}#darwinConfigurations.{args.host}.config.home-manager.users.jfilipe"
    )

    def config_file(name):
        return run(["nix", "eval", "--raw", f'{prefix}.home.file."{name}".text']).stdout

    with tempfile.TemporaryDirectory(prefix="obelisk-runtime-") as tmp:
        root = Path(tmp)
        home = root / "home with spaces"
        home.mkdir()
        env = dict(os.environ)
        # Prevent inherited sessions, hooks, tmux and config overrides from
        # directing the test toward any live application state.
        for key in list(env):
            if key.startswith(("ATUIN_", "XDG_", "OPAM")) or key in (
                "TMUX",
                "ZDOTDIR",
                "NU_LIB_DIRS",
                "NU_PLUGIN_DIRS",
            ):
                env.pop(key)
        env.update(
            HOME=str(home),
            XDG_CONFIG_HOME=str(home / ".config"),
            XDG_DATA_HOME=str(home / ".local/share"),
            XDG_STATE_HOME=str(home / ".local/state"),
            XDG_CACHE_HOME=str(home / ".cache"),
            ATUIN_CONFIG_DIR=str(home / ".config/atuin"),
            OPAMROOT=str(home / ".opam"),
            PATH=f"{BREW}:/usr/bin:/bin:/usr/sbin:/sbin",
            TERM="xterm-256color",
        )
        atuin_config = Path(env["ATUIN_CONFIG_DIR"])
        atuin_config.mkdir(parents=True)
        (atuin_config / "config.toml").write_text(
            "auto_sync = false\n[daemon]\nenabled = false\n"
        )

        files = {
            "zshrc": "./.zshrc",
            "env.nu": "Library/Application Support/nushell/env.nu",
            "config.nu": "Library/Application Support/nushell/config.nu",
        }
        for target, source in files.items():
            # Generated config intentionally contains the declared home path.
            # Rebase only this test copy, never the actual managed dotfiles.
            (root / target).write_text(
                config_file(source).replace(str(Path.home()), str(home))
            )
        global_prefix = (
            f"{REPO}#darwinConfigurations.{args.host}.config.environment.etc"
        )
        for name in ("bashrc", "zshenv", "zprofile", "zshrc"):
            text = run(
                ["nix", "eval", "--raw", f'{global_prefix}."{name}".text']
            ).stdout
            path = root / f"global-{name}"
            path.write_text(text)
            shell = BREW / ("bash" if name == "bashrc" else "zsh")
            run([str(shell), "-n", str(path)], env=env)
        print("PASS global init: Homebrew Bash/Zsh syntax")

        zshrc = root / "zshrc"
        run([str(BREW / "zsh"), "-n", str(zshrc)], env=env)
        shell_test = r"""
source "$1"
# Exercise autocomplete's first-prompt setup without running Pure's async UI.
.autocomplete__main:precmd
(( $+functions[_atuin_search] )) || exit 10
[[ $(bindkey '^R') == *atuin* ]] || exit 11
(( $+functions[__zoxide_z] )) || exit 12
(( $+functions[y] )) || exit 13
[[ $FZF_CTRL_R_COMMAND == '' ]] || exit 14
zstyle -s ':chpwd:' recent-dirs-file recent_file
[[ $recent_file == "${XDG_DATA_HOME}/zsh/chpwd-recent-dirs" ]] || exit 15
[[ ${chpwd_functions[(Ie)chpwd_recent_dirs]} -gt 0 ]] || exit 16
builtin cd "$HOME/.config"
# chpwd_recent_dirs intentionally ignores -c/eval contexts. Exercise its
# actual reader/writer directly, after verifying the hook was registered.
chpwd_recent_filehandler "$HOME" "$PWD"
chpwd_recent_filehandler
[[ -s $recent_file && $reply[2] == $PWD ]] || exit 17
print 'zsh-ok'
"""
        output = run_terminal(
            [str(BREW / "zsh"), "-d", "-f", "-i", "-c", shell_test, "test", str(zshrc)],
            env,
        )
        assert "zsh-ok" in output
        for error in ("no such file", "not found", "can't change option", "Error:"):
            assert error not in output, output
        print("PASS Zsh: syntax, plugins, recent dirs, Atuin Ctrl-R, zoxide, Yazi, fzf")

        query = """{
            atuin: (which atuin | get path | first),
            zoxide: (which zoxide | get path | first),
            carapace: (which carapace | get path | first),
            keybindings: ($env.config.keybindings | where name =~ atuin | get name),
            completer: ($env.config.completions.external.completer | describe),
            cd: (scope aliases | where name == cd | get expansion | first),
            y: (scope commands | where name == y | get type | first)
        } | to json | print
        # Atuin prepares its index in a background job at startup. Let the
        # disposable test finish that job before exiting the interactive Nu.
        for _ in 1..100 {
            if (job list | is-empty) { break }
            sleep 50ms
        }
        if (job list | is-not-empty) { error make {msg: "Atuin index job timed out"} }
        """
        nu_command = [
            str(BREW / "nu"),
            "--no-history",
            "--env-config",
            str(root / "env.nu"),
            "--config",
            str(root / "config.nu"),
            "-i",
            "-c",
            query,
        ]
        for _ in range(2):
            result = run(nu_command, env=env)
            assert not result.stderr.strip(), result.stderr
            data, end = json.JSONDecoder().raw_decode(result.stdout)
            suffix = result.stdout[end:].strip()
            assert suffix in ("", "\u001b[0 q"), repr(suffix)
            for name in ("atuin", "zoxide", "carapace"):
                assert data[name] == str(BREW / name), data
            bindings = data["keybindings"]
            assert len(bindings) >= 2 and len(bindings) == len(set(bindings)), data
            assert data["completer"] == "closure", data
            assert data["cd"] == "__zoxide_z" and data["y"] == "custom", data
        print(
            "PASS Nushell: cold/warm init, Homebrew tools, unique bindings, completer, cd/y"
        )

        tmux_config = root / "tmux.conf"
        tmux_config.write_text(
            run(
                [
                    "nix",
                    "eval",
                    "--raw",
                    f'{prefix}.xdg.configFile."tmux/tmux.conf".text',
                ]
            ).stdout.replace(str(Path.home()), str(home))
        )
        tmux = [str(BREW / "tmux"), "-S", str(root / "tmux.sock")]
        try:
            run(
                tmux + ["-f", str(tmux_config), "new-session", "-d", "/bin/sleep 30"],
                env=env,
            )
            shell = run(tmux + ["show-options", "-gv", "default-shell"], env=env)
            assert shell.stdout.strip() == str(BREW / "zsh")
        finally:
            # Only this test's private socket; never kill a user's tmux server.
            subprocess.run(
                tmux + ["kill-server"],
                env=env,
                capture_output=True,
                check=False,
                timeout=10,
            )
        print("PASS tmux: generated config and Nix-only plugins on Homebrew tmux/Zsh")

        # Use SQLite's online backup, including any WAL, rather than copying a
        # live DB file. Only the disposable snapshot is opened by Homebrew Atuin.
        history = Path(os.environ.get("XDG_DATA_HOME", Path.home() / ".local/share"))
        history = history / "atuin/history.db"
        if history.is_file():
            snapshot = home / ".local/share/atuin/history.db"
            snapshot.parent.mkdir(parents=True, exist_ok=True)
            with (
                closing(
                    sqlite3.connect(f"{history.as_uri()}?mode=ro", uri=True)
                ) as source,
                closing(sqlite3.connect(snapshot)) as dest,
            ):
                source.backup(dest)
                count = dest.execute("select count(*) from history").fetchone()[0]
            result = run(
                [str(BREW / "atuin"), "history", "list", "--format", "{uuid}"],
                env=dict(env, ATUIN_SESSION="0" * 32),
            )
            assert len(result.stdout.splitlines()) == count
            print(
                f"PASS Atuin: Homebrew reads all {count} entries from an isolated snapshot"
            )
        else:
            print("SKIP Atuin database compatibility: no existing history database")


if __name__ == "__main__":
    main()
