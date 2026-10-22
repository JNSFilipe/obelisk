#!/usr/bin/env python3
"""Expose Pi-owned Jig skills to Claude Code/Codex without copying skill files."""

import sys
from pathlib import Path


def share_jig(skills_directory, home):
    if not skills_directory.exists() and not skills_directory.parent.exists():
        print("Jig sharing deferred: start Pi to install Jig, then activate again.")
        return
    skills = sorted(
        path for path in skills_directory.glob("jig-*")
        if path.is_dir() and (path / "SKILL.md").is_file()
    )
    if not skills:
        raise ValueError(f"no Jig skills found in {skills_directory}")

    shared_copies = sorted((home / ".agents/skills").glob("jig-*"))
    if shared_copies:
        raise FileExistsError(
            f"manual shared Jig skills would duplicate Pi's package: {shared_copies[0]}; "
            "compare and migrate them explicitly"
        )

    destinations = [home / tool / "skills" for tool in (".claude", ".codex")]
    links = [
        (destination / skill.name, skill)
        for destination in destinations
        for skill in skills
    ]
    # Preflight the whole operation: a manual installation must be inspected and
    # explicitly removed, never recursively deleted by a routine activation.
    for target, source in links:
        if target.is_symlink() and target.readlink() == source:
            continue
        if target.exists() or target.is_symlink():
            raise FileExistsError(
                f"refusing to replace unmanaged Jig skill: {target}; "
                "compare it with the Pi-managed copy and migrate it explicitly"
            )

    names = {skill.name for skill in skills}
    for destination in destinations:
        for target in destination.glob("jig-*"):
            if (
                target.name not in names
                and target.is_symlink()
                and target.readlink() == skills_directory / target.name
            ):
                target.unlink()  # Only links owned by this helper, never skill files.

    for target, source in links:
        if not target.is_symlink():
            target.parent.mkdir(parents=True, exist_ok=True)
            target.symlink_to(source, target_is_directory=True)


def main():
    if len(sys.argv) != 3:
        print(f"usage: {sys.argv[0]} PI_JIG_SKILLS_DIRECTORY HOME", file=sys.stderr)
        return 2
    try:
        share_jig(Path(sys.argv[1]).absolute(), Path(sys.argv[2]).absolute())
    except (OSError, ValueError) as error:
        print(f"Jig skill sharing: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
