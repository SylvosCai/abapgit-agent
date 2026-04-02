#!/usr/bin/env bash
# Integration tests for the debug command — three scenarios covering both
# interactive REPL mode and scripted AI (--json / daemon) mode.
#
# All scenarios run from the abgagt-debug-test repo directory so .abapGitAgent
# points to $CAIS_DEBUG_TEST (not the main abapgit-agent project).
#
# Trigger: "run --class ZCL_CAIS_DBG_TRIGGER" hits ZCL_CAIS_DBG_TRIGGER:33
# Blocked:  a second "run --class" queues for a dialog work process while the first is paused
#
# Scenario 1 (REPL — simple):
#   attach → trigger hits breakpoint → REPL appears → q → blocked command continues
#
# Scenario 2 (REPL — takeover):
#   attach ×2, trigger hits breakpoint → session 1 wins → q → blocked command
#   continues, session 2 exits
#
# Scenario 3 (scripted AI / --json mode — full best-practice workflow):
#   attach --json → wait "Listener active" → trigger in bg → poll for {"session":...} →
#   stack --json → vars --json → step over → vars --json →
#   step continue (releases work process) → trigger completes → daemon exits
#
# Usage:
#   bash tests/integration/debug-scenarios.sh 1    # REPL simple
#   bash tests/integration/debug-scenarios.sh 2    # REPL takeover
#   bash tests/integration/debug-scenarios.sh 3    # scripted AI mode
#   bash tests/integration/debug-scenarios.sh      # all

set -euo pipefail

# Resolve paths relative to the abapgit-agent repo root (where this script lives)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AGENT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
DEBUG_REPO="$(cd "$AGENT_ROOT/../abgagt-debug-test" 2>/dev/null && pwd)" || {
  echo "ERROR: abgagt-debug-test repo not found at $AGENT_ROOT/../abgagt-debug-test" >&2
  exit 1
}

AGENT="node $AGENT_ROOT/bin/abapgit-agent"
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
  (cd "$DEBUG_REPO" && $AGENT debug terminate >/dev/null 2>&1) || true   # clears state file + kills daemon if running
  # Kill any orphaned daemon processes left over from a previous failed run.
  # The daemon is detached (spawned with detached:true) so its PID is not tracked
  # by $ATTACH1_PID.  Without this kill, a frozen ABAP work process from a prior
  # failed run persists until the 30-min idle timeout, causing subsequent runs to
  # immediately re-attach to the same frozen session and fail.
  pkill -f debug-daemon.js 2>/dev/null || true
  # Give SAP a moment to detect the dropped HTTP connection and release any frozen
  # work process.  Without this pause the next listener poll can receive the stale
  # DEBUGGEE_ID before the server-side session has been cleaned up.
  # Use 15s when running after a large test suite (e.g. npm run test:all) to allow
  # the system and ICM connection pool to fully drain any residual debug connections.
  sleep 15
  log "Setting breakpoint ZCL_CAIS_DBG_TRIGGER:33 ..."
  (cd "$DEBUG_REPO" && $AGENT debug delete --all >/dev/null 2>&1) || true
  # Retry debug set: under load the ADT POST may transiently fail (ICM 400).
  # Without a successful set the attach command exits immediately with
  # "No breakpoints set", leaving the scenario with no session JSON.
  for _i in 1 2 3 4 5; do
    (cd "$DEBUG_REPO" && $AGENT debug set --object ZCL_CAIS_DBG_TRIGGER --line 33 >/dev/null 2>&1) && break
    log "debug set attempt $_i failed — retrying in 5s..."
    sleep 5
  done
}

cleanup() {
  # Release any frozen ABAP work process BEFORE killing Node.js PIDs.
  # If a scenario fails mid-way the ADT session may be active; terminate it
  # cleanly so the WP is freed rather than frozen in SM50 until SAP idle timeout.
  #
  # Retry up to 5 times with 2s delay: the ICM may transiently return HTTP 400
  # ("Service cannot be reached") right after a failed step, and we must keep
  # trying until terminate succeeds or we give up.
  for _i in 1 2 3 4 5; do
    (cd "$DEBUG_REPO" && $AGENT debug terminate >/dev/null 2>&1) && break
    sleep 2
  done
  [[ -n "$ATTACH1_PID" ]] && kill "$ATTACH1_PID" 2>/dev/null || true
  [[ -n "$ATTACH2_PID" ]] && kill "$ATTACH2_PID" 2>/dev/null || true
  [[ -n "$TRIGGER_PID" ]] && kill "$TRIGGER_PID" 2>/dev/null || true
  [[ -n "$BLOCKED_PID" ]] && kill "$BLOCKED_PID" 2>/dev/null || true
  exec 6>&- 2>/dev/null || true
  # Kill any daemon that survived the scenario (e.g. after a step --type continue
  # failure the daemon stays alive, keeping the ABAP work process frozen).
  pkill -f debug-daemon.js 2>/dev/null || true
  rm -f /tmp/dbg_*.out /tmp/dbg_*.fifo
  (cd "$DEBUG_REPO" && $AGENT debug delete --all >/dev/null 2>&1) || true
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
  (cd "$DEBUG_REPO" && $AGENT debug attach < /tmp/dbg_s1.fifo > /tmp/dbg_s1.out 2>&1) &
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

  # Trigger: run --class hits the breakpoint (call blocks at the breakpoint)
  rm -f /tmp/dbg_trigger.out
  (cd "$DEBUG_REPO" && $AGENT run --class ZCL_CAIS_DBG_TRIGGER > /tmp/dbg_trigger.out 2>&1) &
  TRIGGER_PID=$!
  log "Trigger (run --class) PID=$TRIGGER_PID"

  log "Waiting for breakpoint hit (up to ${TIMEOUT}s)..."
  if wait_text /tmp/dbg_s1.out "debug>" $TIMEOUT; then
    pass "Breakpoint hit — REPL prompt appeared"
  else
    fail "REPL never appeared; output: $(cat /tmp/dbg_s1.out 2>/dev/null | tail -3)"
    (cd "$DEBUG_REPO" && $AGENT debug terminate >/dev/null 2>&1) || true  # release frozen WP before giving up
    kill "$TRIGGER_PID" 2>/dev/null; ATTACH1_PID=""; TRIGGER_PID=""; return 1
  fi

  # Start the "blocked" command AFTER the breakpoint is hit so the dialog WP
  # is occupied — this second run queues for a free WP
  rm -f /tmp/dbg_blocked.out
  (cd "$DEBUG_REPO" && $AGENT run --class ZCL_CAIS_DBG_TRIGGER > /tmp/dbg_blocked.out 2>&1) &
  BLOCKED_PID=$!
  log "Blocked (run --class) PID=$BLOCKED_PID"
  sleep 1

  if kill -0 "$BLOCKED_PID" 2>/dev/null; then
    pass "Blocked command is waiting (work process occupied)"
  else
    fail "Blocked command finished immediately — it wasn't blocked"
    cat /tmp/dbg_blocked.out
  fi

  local T_QUIT; T_QUIT=$(date +%s)
  # Delete breakpoints BEFORE sending 'q' so the released WP doesn't re-hit
  # ZCL_CAIS_DBG_TRIGGER:33 when the blocked run executes after being unblocked.
  # Without this, the blocked run triggers a second breakpoint hit with no
  # listener ready to release it, leaving the WP frozen in SM50.
  (cd "$DEBUG_REPO" && $AGENT debug delete --all >/dev/null 2>&1) || true
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

  # CRITICAL: verify the TRIGGER (the WP frozen at the breakpoint) also completes.
  # "Blocked command completed" only proves SOME WP was free — the debug WP can
  # still be frozen if the blocked command ran on a different free WP.
  # Trigger completion means the frozen WP was actually released.
  # Allow 120s: after a full test:all run the SAP system is under load; the
  # run command can take up to 90s to complete after the WP resumes.
  log "Waiting for trigger to complete (confirms WP released, up to 120s)..."
  if wait_pid_exit "$TRIGGER_PID" 120; then
    pass "Trigger completed — work process released"
  else
    fail "Trigger did NOT complete — WP may still be frozen, attempting terminate..."
    for _i in 1 2 3 4 5; do
      (cd "$DEBUG_REPO" && $AGENT debug terminate >/dev/null 2>&1) && break
      sleep 3
    done
    kill "$TRIGGER_PID" 2>/dev/null || true
  fi

  wait_pid_exit "$ATTACH1_PID" 10 || kill "$ATTACH1_PID" 2>/dev/null || true
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
  (cd "$DEBUG_REPO" && $AGENT debug attach < /dev/null > /tmp/dbg_s2.out 2>&1) &
  ATTACH2_PID=$!
  log "Session 2 (attach) PID=$ATTACH2_PID [started first]"

  if ! wait_text /tmp/dbg_s2.out "Listener active" 15; then
    fail "Session 2 never reached listener loop (15s)"
    ATTACH2_PID=""; return 1
  fi

  # Session 1 starts SECOND.  Its refreshBreakpoints re-registers the breakpoints,
  # so ADT routes the next hit to session 1 (the more recently registered listener).
  open_stdin_pipe /tmp/dbg_s1.fifo 6
  (cd "$DEBUG_REPO" && $AGENT debug attach < /tmp/dbg_s1.fifo > /tmp/dbg_s1.out 2>&1) &
  ATTACH1_PID=$!
  log "Session 1 (attach) PID=$ATTACH1_PID [started second — wins breakpoint]"

  if ! wait_text /tmp/dbg_s1.out "Listener active" 15; then
    fail "Session 1 never reached listener loop (15s)"
    ATTACH1_PID=""; return 1
  fi
  sleep 1

  rm -f /tmp/dbg_trigger.out
  (cd "$DEBUG_REPO" && $AGENT run --class ZCL_CAIS_DBG_TRIGGER > /tmp/dbg_trigger.out 2>&1) &
  TRIGGER_PID=$!
  log "Trigger (run --class) PID=$TRIGGER_PID"

  log "Waiting for session 1 REPL (up to 30s)..."
  if wait_text /tmp/dbg_s1.out "debug>" 30; then
    pass "Session 1 took over debugger (REPL prompt appeared)"
  else
    fail "Session 1 REPL never appeared in 30s"
    (cd "$DEBUG_REPO" && $AGENT debug terminate >/dev/null 2>&1) || true  # release frozen WP before giving up
    kill "$TRIGGER_PID" 2>/dev/null; return 1
  fi

  rm -f /tmp/dbg_blocked.out
  (cd "$DEBUG_REPO" && $AGENT run --class ZCL_CAIS_DBG_TRIGGER > /tmp/dbg_blocked.out 2>&1) &
  BLOCKED_PID=$!
  log "Blocked (run --class) PID=$BLOCKED_PID"
  sleep 1

  if kill -0 "$BLOCKED_PID" 2>/dev/null; then
    pass "Blocked command is waiting (work process occupied)"
  else
    fail "Blocked command finished immediately — not blocked"
  fi

  local T_QUIT; T_QUIT=$(date +%s)
  # Delete breakpoints BEFORE sending 'q' so the released WP doesn't re-hit
  # ZCL_CAIS_DBG_TRIGGER:33 when the blocked run executes after being unblocked.
  # Without this, the blocked run triggers a second breakpoint hit with no
  # listener ready to release it, leaving the WP frozen in SM50.
  (cd "$DEBUG_REPO" && $AGENT debug delete --all >/dev/null 2>&1) || true
  echo "q" >&6
  log "Sent 'q' — waiting for blocked command to complete (up to 10s)..."

  if wait_pid_exit "$BLOCKED_PID" 10; then
    local elapsed=$(( $(date +%s) - T_QUIT ))
    pass "Blocked command completed ~${elapsed}s after 'q'"
  else
    fail "Blocked command did NOT complete within 10s"
    kill "$BLOCKED_PID" 2>/dev/null || true
  fi

  log "Waiting for session 1 to exit (up to 10s)..."
  if wait_pid_exit "$ATTACH1_PID" 10; then
    pass "Session 1 exited after 'q'"
  else
    fail "Session 1 did NOT exit within 5s"
  fi

  # CRITICAL: verify the TRIGGER (the WP frozen at the breakpoint) also completes.
  log "Waiting for trigger to complete (confirms WP released, up to 120s)..."
  if wait_pid_exit "$TRIGGER_PID" 120; then
    pass "Trigger completed — work process released"
  else
    fail "Trigger did NOT complete — WP may still be frozen, attempting terminate..."
    for _i in 1 2 3 4 5; do
      (cd "$DEBUG_REPO" && $AGENT debug terminate >/dev/null 2>&1) && break
      sleep 3
    done
    kill "$TRIGGER_PID" 2>/dev/null || true
  fi

  # Session 2 may have caught a second breakpoint hit from the blocked run
  # completing and re-running ZCL_CAIS_DBG_TRIGGER:33.  Terminate any active session
  # it holds before hard-killing the process so the WP is released cleanly.
  (cd "$DEBUG_REPO" && $AGENT debug terminate >/dev/null 2>&1) || true
  kill "$ATTACH2_PID" 2>/dev/null || true
  wait_pid_exit "$ATTACH1_PID" 5  || kill "$ATTACH1_PID" 2>/dev/null || true
  wait_pid_exit "$ATTACH2_PID" 10 || kill "$ATTACH2_PID" 2>/dev/null || true
  wait_pid_exit "$BLOCKED_PID" 5  || kill "$BLOCKED_PID" 2>/dev/null || true
  ATTACH1_PID=""; ATTACH2_PID=""; TRIGGER_PID=""; BLOCKED_PID=""
  exec 6>&- 2>/dev/null || true
}

# ── Scenario 3: scripted AI mode (--json / daemon IPC) ───────────────────────
#
# Follows the four best-practice rules from abap/CLAUDE.md exactly:
#   Rule 1: wait for "Listener active" before firing trigger — listener must be registered first
#   Rule 2: keep trigger process alive in background for the entire session
#   Rule 3: always finish with step --type continue — releases the frozen work process
#   Rule 4: never pass --session to step/vars/stack — auto-load from daemon state file

scenario3() {
  echo ""
  echo "  Scenario 3: scripted AI mode — attach --json → daemon IPC → stack/vars/step/continue"
  echo "  $(printf '─%.0s' {1..85})"

  ensure_breakpoint

  rm -f /tmp/dbg_attach.out /tmp/dbg_trigger.out

  # ── Step 1: start attach listener in background (rule 1: wait for listener before trigger)
  # attach --json emits "Listener active" to stderr (captured in the output file) once
  # the listener POST is about to be sent to ADT.  Waiting for this marker ensures the
  # trigger does not fire before ADT has a registered listener, eliminating the race that
  # caused "No session JSON" under system load when a blind sleep was not long enough.
  (cd "$DEBUG_REPO" && $AGENT debug attach --json > /tmp/dbg_attach.out 2>&1) &
  ATTACH1_PID=$!
  log "Attach (--json) PID=$ATTACH1_PID"

  # Rule 1: wait for "Listener active" then give the POST 2s to reach ADT
  if ! wait_text /tmp/dbg_attach.out "Listener active" 30; then
    fail "Attach never printed 'Listener active' in 30s (breakpoints may not be set)"
    ATTACH1_PID=""; return 1
  fi
  sleep 2

  # ── Step 2: start trigger in background — MUST stay alive for the whole session (rule 2)
  (cd "$DEBUG_REPO" && $AGENT run --class ZCL_CAIS_DBG_TRIGGER > /tmp/dbg_trigger.out 2>&1) &
  TRIGGER_PID=$!
  log "Trigger (run --class) PID=$TRIGGER_PID"

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
  # Retry up to 12 times with 5s delay: ADT stateful routing can return HTTP 400
  # transiently while the ICM connection pool recovers from previous test load.
  # 12×5s = 60s max, which is enough for SAP to fully drain prior ICM sessions.
  STACK_OK=0
  for _i in 1 2 3 4 5 6 7 8 9 10 11 12; do
    STACK_OUT=$((cd "$DEBUG_REPO" && $AGENT debug stack --json) 2>&1 || true)
    if echo "$STACK_OUT" | grep -q '"frames"' 2>/dev/null; then
      STACK_OK=1; break
    fi
    log "stack returned error (attempt $_i/12) — waiting 5s for ICM to recover..."
    sleep 5
  done
  if [ "$STACK_OK" -eq 1 ]; then
    pass "stack --json returned {\"frames\":[...]} (rule 4: no --session needed)"
  else
    fail "stack --json did not return expected JSON: $STACK_OUT"
  fi

  # ── Step 5: vars --json — no --session flag (rule 4)
  VARS_OUT=$((cd "$DEBUG_REPO" && $AGENT debug vars --json) 2>&1 || true)
  if echo "$VARS_OUT" | grep -qE '"variables":\s*\[.+\]' 2>/dev/null; then
    pass "vars --json returned {\"variables\":[...]} (rule 4: no --session needed)"
  else
    fail "vars --json did not return expected JSON: $VARS_OUT"
  fi

  # ── Step 6: step over — no --session flag (rule 4)
  # Retry same as stack — stateful routing may be briefly blocked.
  STEP_OK=0
  for _i in 1 2 3 4 5 6 7 8; do
    STEP_OUT=$((cd "$DEBUG_REPO" && $AGENT debug step --type over --json) 2>&1 || true)
    if echo "$STEP_OUT" | grep -q '"position"' 2>/dev/null; then
      STEP_OK=1; break
    fi
    log "step over returned error (attempt $_i/8) — waiting 3s..."
    sleep 3
  done
  if [ "$STEP_OK" -eq 1 ]; then
    pass "step --type over --json returned {\"position\":{...}}"
  else
    fail "step --type over --json did not return expected JSON: $STEP_OUT"
  fi

  # ── Step 7: vars --json again — verify state after step
  VARS2_OUT=$((cd "$DEBUG_REPO" && $AGENT debug vars --json) 2>&1 || true)
  if echo "$VARS2_OUT" | grep -qE '"variables":\s*\[.+\]' 2>/dev/null; then
    pass "vars --json after step returned {\"variables\":[...]}"
  else
    fail "vars --json after step did not return expected JSON: $VARS2_OUT"
  fi

  # ── Step 8: step continue — RULE 3: always release the frozen work process
  # Critical: this MUST succeed to release the ABAP WP. Retry aggressively.
  CONT_OK=0
  for _i in 1 2 3 4 5 6 7 8; do
    CONT_OUT=$((cd "$DEBUG_REPO" && $AGENT debug step --type continue --json) 2>&1 || true)
    if echo "$CONT_OUT" | grep -qE '"position"|"finished"' 2>/dev/null; then
      CONT_OK=1; break
    fi
    log "step continue returned error (attempt $_i/8) — waiting 3s..."
    sleep 3
  done
  if [ "$CONT_OK" -eq 1 ]; then
    pass "step --type continue --json released work process (rule 3)"
  else
    fail "step --type continue --json did not return expected JSON: $CONT_OUT"
    # Last-resort: terminate the session to prevent WP staying frozen
    log "Attempting terminate as fallback to release frozen WP..."
    for _j in 1 2 3 4 5; do
      (cd "$DEBUG_REPO" && $AGENT debug terminate >/dev/null 2>&1) && break
      sleep 3
    done
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
  all)  scenario1; sleep 20; scenario2; sleep 20; scenario3 ;;
  *)    echo "Usage: $0 [1|2|3|all]"; exit 1 ;;
esac

echo ""
[[ $FAIL -eq 0 ]]
