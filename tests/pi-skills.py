#!/usr/bin/env python3
"""Exercise Jig sharing in isolated homes; never touch personal installations."""

import importlib.util
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(os.environ.get(
    "PI_SHARE_JIG_SCRIPT",
    Path(__file__).resolve().parents[1] / "configs/scripts/pi-share-jig.py",
))
sys.dont_write_bytecode = True
spec = importlib.util.spec_from_file_location("pi_share_jig", SCRIPT)
sharing = importlib.util.module_from_spec(spec)
spec.loader.exec_module(sharing)


class ShareJigTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="pi-skills-test-")
        self.addCleanup(self.tmp.cleanup)
        self.home = Path(self.tmp.name) / "home with spaces"
        self.source = self.home / ".pi/agent/git/github.com/JNSFilipe/jig/skills"
        for name in ("jig-apply", "jig-close"):
            self.skill(name)

    def skill(self, name):
        directory = self.source / name
        directory.mkdir(parents=True)
        (directory / "SKILL.md").write_text(f"---\nname: {name}\n---\nSkill\n")
        return directory

    def share(self):
        sharing.share_jig(self.source, self.home)

    def test_links_resolve_to_one_copy_and_are_idempotent(self):
        self.share()
        links = [self.home / tool / "skills" / name for tool in (".claude", ".codex")
                 for name in ("jig-apply", "jig-close")]
        before = {link: link.lstat().st_ino for link in links}
        for link in links:
            self.assertTrue(link.is_symlink())
            self.assertEqual(link.resolve(), (self.source / link.name).resolve())
            self.assertEqual((link / "SKILL.md").read_text(),
                             (self.source / link.name / "SKILL.md").read_text())
        self.share()
        self.assertEqual(before, {link: link.lstat().st_ino for link in links})
        self.assertFalse((self.home / ".agents").exists())

    def test_missing_source_defers_without_touching_manual_copies(self):
        missing_home = Path(self.tmp.name) / "fresh"
        copy = missing_home / ".agents/skills/jig-apply/SKILL.md"
        copy.parent.mkdir(parents=True)
        copy.write_text("local")
        sharing.share_jig(missing_home / ".pi/agent/git/missing/skills", missing_home)
        self.assertEqual(copy.read_text(), "local")
        self.assertFalse((missing_home / ".claude").exists())

    def test_empty_installed_source_is_an_error(self):
        empty = Path(self.tmp.name) / "empty"
        empty.mkdir()
        with self.assertRaisesRegex(ValueError, "no Jig skills"):
            sharing.share_jig(empty, self.home)
        self.assertFalse((self.home / ".claude").exists())

    def test_installed_checkout_without_a_skills_directory_is_an_error(self):
        checkout = Path(self.tmp.name) / "installed-checkout"
        checkout.mkdir()
        with self.assertRaisesRegex(ValueError, "no Jig skills"):
            sharing.share_jig(checkout / "skills", self.home)
        self.assertFalse((self.home / ".claude").exists())

    def test_manual_destination_conflict_preflights_all_links(self):
        conflict = self.home / ".codex/skills/jig-close/SKILL.md"
        conflict.parent.mkdir(parents=True)
        conflict.write_text("custom")
        with self.assertRaisesRegex(FileExistsError, "unmanaged Jig skill"):
            self.share()
        self.assertEqual(conflict.read_text(), "custom")
        self.assertFalse((self.home / ".claude").exists())

    def test_foreign_and_dangling_links_are_not_overwritten(self):
        target = self.home / ".codex/skills/jig-close"
        target.parent.mkdir(parents=True)
        for source in (self.source / "jig-apply", Path(self.tmp.name) / "missing"):
            with self.subTest(source=source):
                target.symlink_to(source)
                with self.assertRaises(FileExistsError):
                    self.share()
                self.assertEqual(target.readlink(), source)
                self.assertFalse((self.home / ".claude").exists())
                target.unlink()

    def test_shared_copies_cannot_silently_duplicate_pi_skills(self):
        copy = self.home / ".agents/skills/jig-apply/SKILL.md"
        copy.parent.mkdir(parents=True)
        copy.write_text("original")
        with self.assertRaisesRegex(FileExistsError, "duplicate Pi's package"):
            self.share()
        self.assertEqual(copy.read_text(), "original")
        self.assertFalse((self.home / ".claude").exists())

    def test_unrelated_resources_and_unowned_stale_links_are_untouched(self):
        self.skill("another-skill")
        unrelated = self.home / ".claude/skills/unrelated/SKILL.md"
        unrelated.parent.mkdir(parents=True)
        unrelated.write_text("keep me")
        foreign = self.home / ".claude/skills/jig-external"
        foreign.symlink_to(self.source / "another-skill")
        self.share()
        self.assertEqual(unrelated.read_text(), "keep me")
        self.assertEqual(foreign.readlink(), self.source / "another-skill")
        self.assertFalse((self.home / ".codex/skills/another-skill").exists())

    def test_only_stale_owned_links_are_pruned(self):
        target = self.home / ".claude/skills/jig-retired"
        target.parent.mkdir(parents=True)
        target.symlink_to(self.source / "jig-retired")
        manual = self.home / ".claude/skills/jig-local/SKILL.md"
        manual.parent.mkdir()
        manual.write_text("local-only")
        self.share()
        self.assertFalse(target.is_symlink())
        self.assertEqual(manual.read_text(), "local-only")

    def test_cli_reports_usage_and_conflicts(self):
        usage = subprocess.run([sys.executable, str(SCRIPT)], capture_output=True)
        self.assertEqual(usage.returncode, 2)
        conflict = self.home / ".claude/skills/jig-apply"
        conflict.mkdir(parents=True)
        result = subprocess.run([sys.executable, str(SCRIPT), str(self.source), str(self.home)],
                                capture_output=True, text=True)
        self.assertEqual(result.returncode, 1)
        self.assertIn("refusing to replace unmanaged", result.stderr)


if __name__ == "__main__":
    unittest.main()
