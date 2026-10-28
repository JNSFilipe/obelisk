#!/usr/bin/env python3
"""Isolated tests for the Pi settings merger; never loads extensions."""

import importlib.util
import json
import os
import stat
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

sys.dont_write_bytecode = True

SCRIPT = Path(
    os.environ.get(
        "PI_SYNC_SETTINGS_SCRIPT",
        Path(__file__).resolve().parents[1] / "configs/scripts/pi-sync-settings.py",
    )
)
PACKAGES = ["npm:example-one", "npm:@example/two"]
DEFAULTS = {
    "defaultProvider": "example",
    "defaultModel": "declared-model",
    "defaultThinkingLevel": "high",
}
DECLARED = {"packages": PACKAGES, "defaults": DEFAULTS}


class PiSettingsTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="pi settings test ")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.manifest = self.root / "settings-manifest.json"
        self.manifest.write_text(json.dumps(DECLARED))
        self.settings = self.root / ".pi/agent/settings.json"

    def merge(self, succeeds=True):
        result = subprocess.run(
            [sys.executable, str(SCRIPT), str(self.manifest), str(self.settings)],
            capture_output=True,
            text=True,
            timeout=10,
            check=False,
        )
        if succeeds:
            self.assertEqual(result.returncode, 0, result.stderr)
        else:
            self.assertNotEqual(result.returncode, 0)
        self.assertFalse(list(self.settings.parent.glob(".pi-settings-*")))
        return result

    def write_settings(self, value):
        self.settings.parent.mkdir(parents=True, exist_ok=True)
        self.settings.write_text(json.dumps(value))

    def test_create_missing_settings(self):
        self.merge()
        self.assertEqual(
            json.loads(self.settings.read_text()), DEFAULTS | {"packages": PACKAGES}
        )
        self.assertEqual(stat.S_IMODE(self.settings.stat().st_mode), 0o600)
        self.assertFalse(self.settings.is_symlink())

    def test_preserve_preferences_and_local_data(self):
        original = {
            "packages": ["npm:old"],
            "deviceId": "local-only",
            "defaultProvider": "other",
            "defaultModel": "local-model",
            "theme": "custom",
            "nested": {"preserve": [1, False]},
        }
        self.write_settings(original)
        auth = self.settings.parent / "auth.json"
        auth.write_text("auth sentinel")
        session = self.settings.parent / "sessions/example.jsonl"
        session.parent.mkdir()
        session.write_text("session sentinel")
        self.settings.chmod(0o640)
        self.merge()
        self.assertEqual(
            json.loads(self.settings.read_text()),
            original | {"packages": PACKAGES, "defaultThinkingLevel": "high"},
        )
        self.assertEqual(stat.S_IMODE(self.settings.stat().st_mode), 0o640)
        self.assertEqual(auth.read_text(), "auth sentinel")
        self.assertEqual(session.read_text(), "session sentinel")

    def test_idempotent_does_not_rewrite(self):
        self.write_settings({"packages": PACKAGES, "theme": "custom"} | DEFAULTS)
        before = self.settings.stat()
        content = self.settings.read_bytes()
        self.merge()
        after = self.settings.stat()
        self.assertEqual(before.st_ino, after.st_ino)
        self.assertEqual(before.st_mtime_ns, after.st_mtime_ns)
        self.assertEqual(self.settings.read_bytes(), content)

    def test_defaults_fill_only_unset_keys(self):
        self.write_settings(
            {"packages": PACKAGES, "defaultModel": "chosen-in-pi", "theme": "custom"}
        )
        self.merge()
        self.assertEqual(
            json.loads(self.settings.read_text()),
            {
                "packages": PACKAGES,
                "defaultProvider": "example",
                "defaultModel": "chosen-in-pi",
                "defaultThinkingLevel": "high",
                "theme": "custom",
            },
        )

    def test_empty_list_removes_declarations_not_other_settings(self):
        self.manifest.write_text(json.dumps({"packages": []}))
        self.write_settings({"packages": PACKAGES, "theme": "custom"})
        self.merge()
        self.assertEqual(
            json.loads(self.settings.read_text()), {"packages": [], "theme": "custom"}
        )

    def test_invalid_settings_untouched(self):
        self.settings.parent.mkdir(parents=True)
        for content in ("{bad", "[]", "null", "42", '"string"'):
            with self.subTest(content=content):
                self.settings.write_text(content)
                self.merge(succeeds=False)
                self.assertEqual(self.settings.read_text(), content)

    def test_invalid_manifest_untouched(self):
        self.write_settings({"packages": ["npm:old"], "theme": "custom"})
        before = self.settings.read_bytes()
        for content in (
            "{bad",
            "{}",
            "null",
            json.dumps(PACKAGES),
            '{"packages": [1]}',
            '{"packages": [" "]}',
            '{"packages": [], "defaults": []}',
            '{"packages": [], "defaults": {"defaultModel": ""}}',
            '{"packages": [], "defaults": {"defaultModel": 1}}',
        ):
            with self.subTest(content=content):
                self.manifest.write_text(content)
                self.merge(succeeds=False)
                self.assertEqual(self.settings.read_bytes(), before)

    def test_failed_atomic_replace_preserves_original_and_cleans_up(self):
        self.write_settings({"packages": ["npm:old"]})
        before = self.settings.read_bytes()
        spec = importlib.util.spec_from_file_location("pi_sync_settings", SCRIPT)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        with patch.object(
            module.os, "replace", side_effect=OSError("simulated failure")
        ), self.assertRaises(OSError):
            module.sync_settings(self.manifest, self.settings)
        self.assertEqual(self.settings.read_bytes(), before)
        self.assertFalse(list(self.settings.parent.glob(".pi-settings-*")))
        self.assertFalse(Path(str(self.settings) + ".lock").exists())

    def test_busy_lock_refused_without_changes(self):
        self.write_settings({"packages": ["npm:old"]})
        before = self.settings.read_bytes()
        lock = Path(str(self.settings) + ".lock")
        lock.mkdir()
        self.merge(succeeds=False)
        self.assertEqual(self.settings.read_bytes(), before)
        self.assertTrue(lock.is_dir())

    def test_symlink_refused(self):
        self.settings.parent.mkdir(parents=True)
        target = self.root / "target.json"
        target.write_text('{"theme":"untouched"}')
        self.settings.symlink_to(target)
        self.merge(succeeds=False)
        self.assertTrue(self.settings.is_symlink())
        self.assertEqual(target.read_text(), '{"theme":"untouched"}')
        target.unlink()
        self.merge(succeeds=False)
        self.assertTrue(self.settings.is_symlink())
        self.assertFalse(target.exists())


if __name__ == "__main__":
    unittest.main()
