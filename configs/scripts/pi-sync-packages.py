#!/usr/bin/env python3
"""Merge declarative Pi packages into writable, otherwise machine-local settings."""

import json
import os
import stat
import sys
import tempfile
from pathlib import Path


def sync_packages(manifest_path, settings_path):
    packages = json.loads(manifest_path.read_text())
    if not isinstance(packages, list) or not all(
        isinstance(source, str) and source.strip() for source in packages
    ):
        raise ValueError("package manifest must be an array of nonempty strings")

    # Cooperate with Pi's proper-lockfile settings writer (realpath: false).
    # Never steal a busy/stale lock; fail safely and let activation be retried.
    settings_path.parent.mkdir(parents=True, exist_ok=True)
    lock_path = Path(str(settings_path) + ".lock")
    lock_path.mkdir()
    try:
        merge_settings(packages, settings_path)
    finally:
        lock_path.rmdir()


def merge_settings(packages, settings_path):
    if settings_path.is_symlink():
        raise ValueError(f"refusing symlink settings file: {settings_path}")
    try:
        settings = json.loads(settings_path.read_text())
        mode = stat.S_IMODE(settings_path.stat().st_mode)
    except FileNotFoundError:
        settings = {}
        mode = 0o600
    if not isinstance(settings, dict):
        raise TypeError("Pi settings must contain a JSON object")
    if settings.get("packages") == packages:
        return

    settings["packages"] = packages
    content = json.dumps(settings, indent=2, ensure_ascii=False) + "\n"
    temporary = None
    try:
        with tempfile.NamedTemporaryFile(
            mode="w",
            encoding="utf-8",
            dir=settings_path.parent,
            prefix=".pi-settings-",
            delete=False,
        ) as output:
            temporary = Path(output.name)
            output.write(content)
            output.flush()
            os.fsync(output.fileno())
            os.fchmod(output.fileno(), mode)
        os.replace(temporary, settings_path)
    finally:
        if temporary is not None:
            temporary.unlink(missing_ok=True)


def main():
    if len(sys.argv) != 3:
        print(f"usage: {sys.argv[0]} MANIFEST_JSON SETTINGS_JSON", file=sys.stderr)
        return 2
    try:
        sync_packages(Path(sys.argv[1]), Path(sys.argv[2]))
    except (OSError, ValueError, TypeError) as error:
        print(f"Pi package settings: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
