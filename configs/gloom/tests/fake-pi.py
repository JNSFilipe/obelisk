#!/usr/bin/env python3
"""Offline stand-in for `pi --mode rpc`, enough for Pilish and M-x Machina.

Reads one JSON command per line and answers with Pi's response/event shapes.
FAKE_PI_LOG (required) receives one JSON line per argv and command, so tests
can check what Gloom sent.  A prompt containing "ask" first opens a confirm
dialog and waits for its extension_ui_response.  FAKE_PI_SAVED lists session
IDs that have saved history; FAKE_PI_WRONG_ID makes get_state report another
session ID.
"""

import json
import os
import sys
import time

argv = sys.argv[1:]
log_path = os.environ["FAKE_PI_LOG"]
session_id = argv[argv.index("--session-id") + 1] if "--session-id" in argv else "no-id"
saved = os.environ.get("FAKE_PI_SAVED", "").split()
history = [
    {"role": "user", "content": [{"type": "text", "text": "earlier question"}]},
    {"role": "assistant", "content": [{"type": "text", "text": "earlier answer"}]},
] if session_id in saved else []


def log(entry):
    with open(log_path, "a", encoding="utf-8") as output:
        output.write(json.dumps(entry) + "\n")


def send(message):
    sys.stdout.write(json.dumps(message) + "\n")
    sys.stdout.flush()


def respond(command, data=None):
    send({"type": "response", "id": command.get("id"), "command": command["type"],
          "success": True, "data": data if data is not None else {}})


def state():
    reported = os.environ.get("FAKE_PI_WRONG_ID") or session_id
    return {"sessionId": reported,
            "sessionFile": f"/tmp/fake-pi-sessions/{reported}.jsonl",
            "messageCount": len(history), "pendingMessageCount": 0,
            "isStreaming": False, "isCompacting": False, "thinkingLevel": "off",
            "model": {"id": "fake-model", "name": "Fake Model", "provider": "fake"}}


def run_prompt(text, lines):
    user = {"role": "user", "content": [{"type": "text", "text": text}]}
    send({"type": "agent_start"})
    send({"type": "message_start", "message": user})
    send({"type": "message_end", "message": user})
    if "ask" in text:
        send({"type": "extension_ui_request", "id": "dialog-1", "method": "confirm",
              "title": "Fake", "message": "Proceed?"})
        for line in lines:
            reply = json.loads(line)
            log({"command": reply})
            if reply.get("type") == "extension_ui_response":
                break
    time.sleep(0.3)
    reply = {"role": "assistant", "content": [{"type": "text", "text": "fake reply"}]}
    send({"type": "message_start", "message": {"role": "assistant", "content": []}})
    send({"type": "message_update", "message": reply,
          "assistantMessageEvent": {"type": "text_delta", "delta": "fake reply"}})
    send({"type": "message_end", "message": reply})
    history.extend([user, reply])
    send({"type": "agent_end", "messages": history})
    send({"type": "agent_settled"})


def main():
    log({"argv": argv, "cwd": os.getcwd()})
    lines = iter(sys.stdin.readline, "")
    for line in lines:
        if not line.strip():
            continue
        command = json.loads(line)
        log({"command": command})
        kind = command.get("type")
        if kind == "get_state":
            respond(command, state())
        elif kind == "get_messages":
            respond(command, {"messages": history})
        elif kind == "get_commands":
            respond(command, {"commands": []})
        elif kind == "prompt":
            respond(command)
            run_prompt(command.get("message", ""), lines)
        else:
            respond(command)


if __name__ == "__main__":
    main()
