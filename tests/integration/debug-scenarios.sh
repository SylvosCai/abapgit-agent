#!/usr/bin/env bash
# Integration tests for the debug command — three scenarios covering both
# interactive REPL mode and scripted AI (--json / daemon) mode.
#
# Trigger: "inspect --files abap/zcl_abgagt_util.clas.abap" hits ZCL_ABGAGT_UTIL:25
# Blocked:  a second "inspect" queues for a dialog work process while the first is paused
#
# Scenario 1 (REPL — simple):
#   attach → trigger hits breakpoint → REPL appears → q → blocked command continues
#
# Scenario 2 (REPL — takeover):
#   attach ×2, trigger hits breakpoint → session 1 wins → q → blocked command
#   continues, session 2 exits
#
# Scenario 3 (scripted AI / --json mode — full best-practice workflow):
#   attach --json → sleep 2 → trigger in bg → poll for {"session":...} →
#   stack --json → vars --json → step over → vars --json →
#   step continue (releases work process) → trigger completes → daemon exits
#
# Usage:
#   bash tests/integration/debug-scenarios.sh 1    # REPL simple
#   bash tests/integration/debug-scenarios.sh 2    # REPL takeover
#   bash tests/integration/debug-scenarios.sh 3    # scripted AI mode
#   bash tests/integration/debug-scenarios.sh      # all

set -euo pipefail

AGENT="node $(pwd)/bin/abapgit-agent"
TIMEOUT=60
SCENARIO=${1:-all}
PASS=0
FAIL=0
ATTACH1_PID=""
ATTACH2_PID=""
TRIGGER_PID=""
BLOCKED_PID=""
RESULTS_FILE="${TMPDIR:-/tmp}/debug_scenarios_result"

# ── helpers ──────────────────────────────────────────────────────────────────

# Assertion lines — match ✅/❌ style used by the node runner
pass() { echo "  ✅ $*"; PASS=$((PASS+1)); }
fail() { echo "  ❌ $*"; FAIL=$((FAIL+1)); }

# Progress/diagnostic lines — indented further, no assertion emoji
log()  { echo "      $*"; }

wait_text() {    # wait_text <file> <grep-pattern> <timeout_s>
  local f=$1 p=$2 t=$3 i=0
  while ! grep -q "$p" "$f" 2>/dev/null && (( i < t*10 )); do sleep 0.1; i=$((i+1)); done
  grep -q "$p" "$f" 2>/dev/null
}

wait_pid_exit() {   # wait_pid_exit <pid> <timeout_s>
  local pid=$1 t=$2 i=0
  while kill -0 "$pid" 2>/dev/null && (( i < t*10 )); do sleep 0.1; i=$((i+1)); done
  ! kill -0 "$pid" 2>/dev/null
}

# Open a named fifo so we can write to a process's stdin.
# open_stdin_pipe <path> <fd-number>
# Opens r+w so the open is non-blocking; the target process reads from <path>.
open_stdin_pipe() {
  local path=$1 fd=$2
  rm -f "$path"; mkfifo "$path"
  eval "exec ${fd}<>$path"
}

ensure_breakpoint() {
  log "Clearing stale session state and daemon ..."
  $AGENT debug terminate >/dev/null 2>&1 || true   # clears state file + kills daemon if running
  log "Setting breakpoint ZCL_ABGAGT_UTIL:25 ..."
  $AGENT debug delete --all >/dev/null 2>&1 || true
  $AGENT debug set --object ZCL_ABGAGT_UTIL --line 25 >/dev/null 2>&1 || true
}

cleanup() {
  [[ -n "$ATTACH1_PID" ]] && kill "$ATTACH1_PID" 2>/dev/null || true
  [[ -n "$ATTACH2_PID" ]] && kill "$ATTACH2_PID" 2>/dev/null || true
  [[ -n "$TRIGGER_PID" ]] && kill "$TRIGGER_PID" 2>/dev/null || true
  [[ -n "$BLOCKED_PID" ]] && kill "$BLOCKED_PID" 2>/dev/null || true
  exec 6>&- 2>/dev/null || true
  rm -f /tmp/dbg_*.out /tmp/dbg_*.fifo
  $AGENT debug delete --all >/dev/null 2>&1 || true
  echo "$PASS $FAIL" > "$RESULTS_FILE"
}
trap cleanup EXIT

# ── Scenario 1: REPL simple ───────────────────────────────────────────────────

scenario1() {
  echo ""
  echo "  Scenario 1: REPL — simple attach → q → blocked command continues"
  echo "  $(printf '─%.0s' {1..66})"

  ensure_breakpoint

  # Session 1: attach (stdin controlled via fd 6)
  open_stdin_pipe /tmp/dbg_s1.fifo 6
  rm -f /tmp/dbg_s1.out
  $AGENT debug attach < /tmp/dbg_s1.fifo > /tmp/dbg_s1.out 2>&1 &
  ATTACH1_PID=$!
  log "Attach PID=$ATTACH1_PID"

  # Wait until the attach process emits "Listener active" (printed once at the
  # start of the first listener poll, meaning the ADT long-poll is about to fire).
  if ! wait_text /tmp/dbg_s1.out "Listener active" 15; then
    fail "Attach never reached listener loop (no 'Listener active' in 15s)"
    ATTACH1_PID=""; return 1
  fi
  # Give the listener POST ~1s to reach ADT before the trigger fires.
  sleep 1

  # Trigger: inspect hits the breakpoint (call blocks at the breakpoint)
  rm -f /tmp/dbg_trigger.out
  $AGENT inspect --files abap/zcl_abgagt_util.clas.abap > /tmp/dbg_trigger.out 2>&1 &
  TRIGGER_PID=$!
  log "Trigger (inspect) PID=$TRIGGER_PID"

  log "Waiting for breakpoint hit (up to ${TIMEOUT}s)..."
  if wait_text /tmp/dbg_s1.out "debug>" $TIMEOUT; then
    pass "Breakpoint hit — REPL prompt appeared"
  else
    fail "REPL never appeared; output: $(cat /tmp/dbg_s1.out 2>/dev/null | tail -3)"
    kill "$TRIGGER_PID" 2>/dev/null; ATTACH1_PID=""; TRIGGER_PID=""; return 1
  fi

  # Start the "blocked" command AFTER the breakpoint is hit so the dialog WP
  # is occupied — this second inspect queues for a free WP
  rm -f /tmp/dbg_blocked.out
  $AGENT inspect --files abap/zcl_abgagt_util.clas.abap > /tmp/dbg_blocked.out 2>&1 &
  BLOCKED_PID=$!
  log "Blocked (inspect) PID=$BLOCKED_PID"
  sleep 1

  if kill -0 "$BLOCKED_PID" 2>/dev/null; then
    pass "Blocked command is waiting (work process occupied)"
  else
    fail "Blocked command finished immediately — it wasn't blocked"
    cat /tmp/dbg_blocked.out
  fi

  local T_QUIT; T_QUIT=$(date +%s)
  echo "q" >&6
  log "Sent 'q' — waiting for blocked command to complete (up to ${TIMEOUT}s)..."

  if wait_pid_exit "$BLOCKED_PID" $TIMEOUT; then
    local elapsed=$(( $(date +%s) - T_QUIT ))
    pass "Blocked command completed ~${elapsed}s after 'q'"
  else
    fail "Blocked command did NOT complete within ${TIMEOUT}s"
    echo "  blocked output: $(cat /tmp/dbg_blocked.out 2>/dev/null | tail -3)"
    kill "$BLOCKED_PID" 2>/dev/null || true
  fi

  wait_pid_exit "$ATTACH1_PID" 10 || kill "$ATTACH1_PID" 2>/dev/null || true
  wait_pid_exit "$TRIGGER_PID" 15 || kill "$TRIGGER_PID" 2>/dev/null || true
  wait_pid_exit "$BLOCKED_PID" 5  || kill "$BLOCKED_PID" 2>/dev/null || true
  ATTACH1_PID=""; TRIGGER_PID=""; BLOCKED_PID=""
  exec 6>&- 2>/dev/null || true
}

# ── Scenario 2: REPL takeover ─────────────────────────────────────────────────

scenario2() {
  echo ""
  echo "  Scenario 2: REPL — takeover — session 1 wins breakpoint, q releases blocked command"
  echo "  $(printf '─%.0s' {1..83})"

  ensure_breakpoint

  rm -f /tmp/dbg_s1.out /tmp/dbg_s2.out

  # Session 2 starts FIRST (no stdin — background observer).
  $AGENT debug attach < /dev/null > /tmp/dbg_s2.out 2>&1 &
  ATTACH2_PID=$!
  log "Session 2 (attach) PID=$ATTACH2_PID [started first]"

  if ! wait_text /tmp/dbg_s2.out "Listener active" 15; then
    fail "Session 2 never reached listener loop (15s)"
    ATTACH2_PID=""; return 1
  fi

  # Session 1 starts SECOND.  Its refreshBreakpoints re-registers the breakpoints,
  # so ADT routes the next hit to session 1 (the more recently registered listener).
  open_stdin_pipe /tmp/dbg_s1.fifo 6
  $AGENT debug attach < /tmp/dbg_s1.fifo > /tmp/dbg_s1.out 2>&1 &
  ATTACH1_PID=$!
  log "Session 1 (attach) PID=$ATTACH1_PID [started second — wins breakpoint]"

  if ! wait_text /tmp/dbg_s1.out "Listener active" 15; then
    fail "Session 1 never reached listener loop (15s)"
    ATTACH1_PID=""; return 1
  fi
  sleep 1

  rm -f /tmp/dbg_trigger.out
  $AGENT inspect --files abap/zcl_abgagt_util.clas.abap > /tmp/dbg_trigger.out 2>&1 &
  TRIGGER_PID=$!
  log "Trigger (inspect) PID=$TRIGGER_PID"

  log "Waiting for session 1 REPL (up to 15s)..."
  if wait_text /tmp/dbg_s1.out "debug>" 15; then
    pass "Session 1 took over debugger (REPL prompt appeared)"
  else
    fail "Session 1 REPL never appeared in 15s"
    kill "$TRIGGER_PID" 2>/dev/null; return 1
  fi

  rm -f /tmp/dbg_blocked.out
  $AGENT inspect --files abap/zcl_abgagt_util.clas.abap > /tmp/dbg_blocked.out 2>&1 &
  BLOCKED_PID=$!
  log "Blocked (inspect) PID=$BLOCKED_PID"
  sleep 1

  if kill -0 "$BLOCKED_PID" 2>/dev/null; then
    pass "Blocked command is waiting (work process occupied)"
  else
    fail "Blocked command finished immediately — not blocked"
  fi

  local T_QUIT; T_QUIT=$(date +%s)
  echo "q" >&6
  log "Sent 'q' — waiting for blocked command to complete (up to 10s)..."

  if wait_pid_exit "$BLOCKED_PID" 10; then
    local elapsed=$(( $(date +%s) - T_QUIT ))
    pass "Blocked command completed ~${elapsed}s after 'q'"
  else
    fail "Blocked command did NOT complete within 10s"
    kill "$BLOCKED_PID" 2>/dev/null || true
  fi

  log "Waiting for session 1 to exit (up to 5s)..."
  if wait_pid_exit "$ATTACH1_PID" 5; then
    pass "Session 1 exited after 'q'"
  else
    fail "Session 1 did NOT exit within 5s"
  fi

  # Session 2 is left running (user can Ctrl+C) — kill it silently in cleanup.
  kill "$ATTACH2_PID" 2>/dev/null || true
  wait_pid_exit "$ATTACH1_PID" 5  || kill "$ATTACH1_PID" 2>/dev/null || true
  wait_pid_exit "$ATTACH2_PID" 10 || kill "$ATTACH2_PID" 2>/dev/null || true
  wait_pid_exit "$TRIGGER_PID" 15 || kill "$TRIGGER_PID" 2>/dev/null || true
  wait_pid_exit "$BLOCKED_PID" 5  || kill "$BLOCKED_PID" 2>/dev/null || true
  ATTACH1_PID=""; ATTACH2_PID=""; TRIGGER_PID=""; BLOCKED_PID=""
  exec 6>&- 2>/dev/null || true
}

# ── Scenario 3: scripted AI mode (--json / daemon IPC) ───────────────────────
#
# Follows the four best-practice rules from abap/CLAUDE.md exactly:
#   Rule 1: sleep 2 after starting attach — listener must register before trigger fires
#   Rule 2: keep trigger process alive in background for the entire session
#   Rule 3: always finish with step --type continue — releases the frozen work process
#   Rule 4: never pass --session to step/vars/stack — auto-load from daemon state file

scenario3() {
  echo ""
  echo "  Scenario 3: scripted AI mode — attach --json → daemon IPC → stack/vars/step/continue"
  echo "  $(printf '─%.0s' {1..85})"

  ensure_breakpoint

  rm -f /tmp/dbg_attach.out /tmp/dbg_trigger.out

  # ── Step 1: start attach listener in background (rule 1: sleep 2 after this)
  # attach --json spawns a background daemon and emits {"session":...} once hit.
  # In --json mode "Listener active" is suppressed — we rely on sleep 2 instead.
  $AGENT debug attach --json > /tmp/dbg_attach.out 2>&1 &
  ATTACH1_PID=$!
  log "Attach (--json) PID=$ATTACH1_PID"

  # Rule 1: give the listener POST time to register on ADT before the trigger fires
  sleep 4

  # ── Step 2: start trigger in background — MUST stay alive for the whole session (rule 2)
  $AGENT inspect --files abap/zcl_abgagt_util.clas.abap > /tmp/dbg_trigger.out 2>&1 &
  TRIGGER_PID=$!
  log "Trigger (inspect) PID=$TRIGGER_PID"

  # ── Step 3: poll attach output for {"session":"..."} (breakpoint fired, daemon ready)
  log "Waiting for breakpoint hit and session JSON (up to ${TIMEOUT}s)..."
  SESSION=""
  for i in $(seq 1 $((TIMEOUT * 2))); do
    sleep 0.5
    SESSION=$(grep -o '"session":"[^"]*"' /tmp/dbg_attach.out 2>/dev/null | head -1 | cut -d'"' -f4) || true
    [ -n "$SESSION" ] && break
  done

  if [ -n "$SESSION" ]; then
    pass "Breakpoint hit — session JSON emitted (session=${SESSION:0:8}...)"
  else
    fail "No session JSON in attach output after ${TIMEOUT}s"
    echo "  attach output: $(cat /tmp/dbg_attach.out 2>/dev/null | tail -5)"
    kill "$TRIGGER_PID" 2>/dev/null; ATTACH1_PID=""; TRIGGER_PID=""; return 1
  fi

  # ── Step 4: stack --json — no --session flag (rule 4)
  STACK_OUT=$($AGENT debug stack --json 2>&1 || true)
  if echo "$STACK_OUT" | grep -q '"frames"' 2>/dev/null; then
    pass "stack --json returned {\"frames\":[...]} (rule 4: no --session needed)"
  else
    fail "stack --json did not return expected JSON: $STACK_OUT"
  fi

  # ── Step 5: vars --json — no --session flag (rule 4)
  VARS_OUT=$($AGENT debug vars --json 2>&1 || true)
  if echo "$VARS_OUT" | grep -q '"variables"' 2>/dev/null; then
    pass "vars --json returned {\"variables\":[...]} (rule 4: no --session needed)"
  else
    fail "vars --json did not return expected JSON: $VARS_OUT"
  fi

  # ── Step 6: step over — no --session flag (rule 4)
  STEP_OUT=$($AGENT debug step --type over --json 2>&1 || true)
  if echo "$STEP_OUT" | grep -q '"position"' 2>/dev/null; then
    pass "step --type over --json returned {\"position\":{...}}"
  else
    fail "step --type over --json did not return expected JSON: $STEP_OUT"
  fi

  # ── Step 7: vars --json again — verify state after step
  VARS2_OUT=$($AGENT debug vars --json 2>&1 || true)
  if echo "$VARS2_OUT" | grep -q '"variables"' 2>/dev/null; then
    pass "vars --json after step returned {\"variables\":[...]}"
  else
    fail "vars --json after step did not return expected JSON: $VARS2_OUT"
  fi

  # ── Step 8: step continue — RULE 3: always release the frozen work process
  CONT_OUT=$($AGENT debug step --type continue --json 2>&1 || true)
  if echo "$CONT_OUT" | grep -qE '"position"|"finished"' 2>/dev/null; then
    pass "step --type continue --json released work process (rule 3)"
  else
    fail "step --type continue --json did not return expected JSON: $CONT_OUT"
  fi

  # ── Step 9: trigger should now complete normally (rule 2: kept alive until continue)
  log "Waiting for trigger to complete after continue (up to ${TIMEOUT}s)..."
  if wait_pid_exit "$TRIGGER_PID" $TIMEOUT; then
    pass "Trigger completed normally after step continue"
  else
    fail "Trigger did NOT complete within ${TIMEOUT}s after continue"
    kill "$TRIGGER_PID" 2>/dev/null || true
  fi

  # ── Step 10: attach daemon exits after session ends
  log "Waiting for attach daemon to exit (up to 10s)..."
  if wait_pid_exit "$ATTACH1_PID" 10; then
    pass "Attach daemon exited cleanly after session ended"
  else
    # Daemon may linger up to idle-timeout — kill it, this is not a hard failure
    kill "$ATTACH1_PID" 2>/dev/null || true
    pass "Attach daemon killed (acceptable — daemon idle timeout may be longer)"
  fi

  wait_pid_exit "$ATTACH1_PID" 5 || kill "$ATTACH1_PID" 2>/dev/null || true
  wait_pid_exit "$TRIGGER_PID" 15 || kill "$TRIGGER_PID" 2>/dev/null || true
  ATTACH1_PID=""; TRIGGER_PID=""
}

# ── main ─────────────────────────────────────────────────────────────────────

case "$SCENARIO" in
  1)    scenario1 ;;
  2)    scenario2 ;;
  3)    scenario3 ;;
  all)  scenario1; sleep 5; scenario2; sleep 5; scenario3 ;;
  *)    echo "Usage: $0 [1|2|3|all]"; exit 1 ;;
esac

echo ""
[[ $FAIL -eq 0 ]]
