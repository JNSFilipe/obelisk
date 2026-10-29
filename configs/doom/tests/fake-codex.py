#!/usr/bin/env python3
"""Offline stand-in for the `codex` CLI, enough for Sheprd's agent tests.

Finds Sheprd's `-c notify=[...]` override and calls that program with each
notification's JSON as its last argument, as Codex does.  FAKE_CODEX_EVENTS
is a space-separated script: `approval` or `turn:THREAD-ID`.  FAKE_AGENT_LOG
(required) receives the argv as one JSON line.  The process then idles at a
prompt until it is killed.
"""

import json
import os
import subprocess
import sys
import time

argv = sys.argv[1:]
with open(os.environ["FAKE_AGENT_LOG"], "a", encoding="utf-8") as log:
    log.write(json.dumps({"program": "codex", "argv": argv, "cwd": os.getcwd()}) + "\n")

notify = None
for index, value in enumerate(argv):
    if value == "-c" and argv[index + 1].startswith("notify="):
        notify = json.loads(argv[index + 1][len("notify="):])

print("fake codex ready")
for step in os.environ.get("FAKE_CODEX_EVENTS", "").split():
    time.sleep(0.3)
    kind, _, thread = step.partition(":")
    payload = ({"type": "approval-requested"} if kind == "approval"
               else {"type": "agent-turn-complete", "thread-id": thread, "turn-id": "1"})
    subprocess.run(notify + [json.dumps(payload)], check=True)
sys.stdout.write("› ")
sys.stdout.flush()
while True:
    time.sleep(1)
