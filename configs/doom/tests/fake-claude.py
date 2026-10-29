#!/usr/bin/env python3
"""Offline stand-in for the `claude` CLI, enough for Sheprd's agent tests.

Reads Sheprd's --settings file and runs its hook commands the way Claude
Code does: the hook's JSON payload on standard input.  FAKE_CLAUDE_EVENTS is a
space-separated script of hook events to emit after startup, each optionally
followed by `:permission` (a permission notification) and separated by short
pauses; SessionStart is always emitted first unless FAKE_CLAUDE_CRASH makes it
exit at once.  FAKE_AGENT_LOG (required)
receives the argv as one JSON line.  The process then idles at a prompt until
it is killed, like an interactive session.
"""

import json
import os
import subprocess
import sys
import time

argv = sys.argv[1:]
with open(os.environ["FAKE_AGENT_LOG"], "a", encoding="utf-8") as log:
    log.write(json.dumps({"program": "claude", "argv": argv, "cwd": os.getcwd()}) + "\n")


def option(name):
    return argv[argv.index(name) + 1] if name in argv else None


session = option("--session-id") or option("--resume")
settings = json.load(open(option("--settings"), encoding="utf-8")) if option("--settings") else {}


def emit(event, **fields):
    payload = json.dumps({"hook_event_name": event, "session_id": session, **fields},
                         indent=1)  # multi-line on purpose
    for group in settings.get("hooks", {}).get(event, []):
        for hook in group.get("hooks", []):
            subprocess.run(["sh", "-c", hook["command"]], input=payload.encode(), check=True)


if os.environ.get("FAKE_CLAUDE_CRASH"):
    print("fake claude crashed")
    sys.exit(3)
emit("SessionStart", source="resume" if option("--resume") else "startup")
print("fake claude ready")
for step in os.environ.get("FAKE_CLAUDE_EVENTS", "").split():
    time.sleep(0.3)
    name, _, kind = step.partition(":")
    if name == "Notification":
        emit(name, message=("Claude needs your permission to use Bash" if kind == "permission"
                            else "Claude is waiting for your input"))
    else:
        emit(name)
sys.stdout.write("> ")
sys.stdout.flush()
while True:
    time.sleep(1)
