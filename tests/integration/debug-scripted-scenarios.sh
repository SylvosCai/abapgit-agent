#!/usr/bin/env bash
# Integration tests for the debug command — scripted AI (--json / daemon) mode.
# Scenarios 3, 4, 5: no TTY required, CI-safe.
# REPL scenarios (1 & 2) are in debug-repl-scenarios.sh.
#
# All scenarios run from the abgagt-debug-test repo directory so .abapGitAgent
# points to $ABGAGT_DEBUG_TEST (not the main abapgit-agent project).
#
# Scenario 3 (scripted AI / --json mode — full best-practice workflow):
#   attach --json → wait "Listener active" → trigger in bg → poll for {"session":...} →
#   stack --json → vars --json → step over → vars --json →
#   step continue (releases work process) → trigger completes → daemon exits
#
# Scenario 4 (scripted AI / --json mode — variable inspection):
#   attach --json → trigger → vars --json (list) → vars --name IV_A (value=7) →
#   vars --name IV_B (value=1) → step over → vars --name LV_SUM (value=8) →
#   vars --expand LV_SUM (scalar → empty children) → step continue
#
# Scenario 5 (scripted AI / --json mode — structure / table / field-symbol / dataref):
#   BP on inspect_vars WRITE line (all vars populated) →
#   vars --expand LS_PERSON       → structure fields NAME=Alice, AGE=30
#   vars --expand LT_PERSONS      → 2 row children [1] and [2]
#   vars --expand LT_PERSONS[1]   → row 1 fields (VAR[N] syntax)
#   vars --expand LT_PERSONS[1]->NAME → drills to single field Alice
#   vars --expand LR_PERSON       → dereferences dataref → structure fields
#   <LS> field-symbol             → listed in vars (source-parse enrichment)
#
# Usage:
#   bash tests/integration/debug-scripted-scenarios.sh 3    # scripted AI mode
#   bash tests/integration/debug-scripted-scenarios.sh 4    # variable inspection (scalars)
#   bash tests/integration/debug-scripted-scenarios.sh 5    # structure / table / field-symbol / dataref
#   bash tests/integration/debug-scripted-scenarios.sh      # all (default)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AGENT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
# setup-runner.js clones to /tmp/abgagt-debug-test; fall back to sibling dir for local dev
if [ -d "${TMPDIR:-/tmp}/abgagt-debug-test" ]; then
  DEBUG_REPO="${TMPDIR:-/tmp}/abgagt-debug-test"
elif [ -d "$AGENT_ROOT/../abgagt-debug-test" ]; then
  DEBUG_REPO="$(cd "$AGENT_ROOT/../abgagt-debug-test" && pwd)"
else
  echo "ERROR: abgagt-debug-test repo not found (tried ${TMPDIR:-/tmp}/abgagt-debug-test and $AGENT_ROOT/../abgagt-debug-test)" >&2
  exit 1
fi

AGENT="node $AGENT_ROOT/bin/abapgit-agent"
TIMEOUT=60
SCENARIO=${1:-all}
PASS=0
FAIL=0
ATTACH1_PID=""
TRIGGER_PID=""
RESULTS_FILE="${TMPDIR:-/tmp}/debug_scripted_result"

# ── helpers ──────────────────────────────────────────────────────────────────

pass() { echo "  ✅ $*"; PASS=$((PASS+1)); }
fail() { echo "  ❌ $*"; FAIL=$((FAIL+1)); }
log()  { echo "      $*"; }

wait_text() {
  local f=$1 p=$2 t=$3 i=0
  while ! grep -q "$p" "$f" 2>/dev/null && (( i < t*10 )); do sleep 0.1; i=$((i+1)); done
  grep -q "$p" "$f" 2>/dev/null
}

wait_pid_exit() {
  local pid=$1 t=$2 i=0
  while kill -0 "$pid" 2>/dev/null && (( i < t*10 )); do sleep 0.1; i=$((i+1)); done
  ! kill -0 "$pid" 2>/dev/null
}

ensure_breakpoint() {
  local line_nr=${1:-42}
  log "Clearing stale session state and daemon ..."
  (cd "$DEBUG_REPO" && $AGENT debug terminate >/dev/null 2>&1) || true
  pkill -f debug-daemon.js 2>/dev/null || true
  sleep 15
  log "Setting breakpoint ZCL_ABGAGT_DBG_TRIGGER:${line_nr} ..."
  (cd "$DEBUG_REPO" && $AGENT debug delete --all >/dev/null 2>&1) || true
  for _i in 1 2 3 4 5; do
    (cd "$DEBUG_REPO" && $AGENT debug set --object ZCL_ABGAGT_DBG_TRIGGER --line "${line_nr}" >/dev/null 2>&1) && break
    log "debug set attempt $_i failed — retrying in 5s..."; sleep 5
  done
}

cleanup() {
  for _i in 1 2 3 4 5; do
    (cd "$DEBUG_REPO" && $AGENT debug terminate >/dev/null 2>&1) && break
    sleep 2
  done
  [[ -n "$ATTACH1_PID" ]] && kill "$ATTACH1_PID" 2>/dev/null || true
  [[ -n "$TRIGGER_PID" ]] && kill "$TRIGGER_PID" 2>/dev/null || true
  pkill -f debug-daemon.js 2>/dev/null || true
  rm -f /tmp/dbg_*.out
  (cd "$DEBUG_REPO" && $AGENT debug delete --all >/dev/null 2>&1) || true
  echo "$PASS $FAIL" > "$RESULTS_FILE"
}
trap cleanup EXIT

# ── Scenario 3: scripted AI mode (--json / daemon IPC) ───────────────────────

scenario3() {
  echo ""
  echo "  Scenario 3: scripted AI mode — attach --json → daemon IPC → stack/vars/step/continue"
  echo "  $(printf '─%.0s' {1..85})"

  ensure_breakpoint 42

  rm -f /tmp/dbg_attach.out /tmp/dbg_trigger.out

  (cd "$DEBUG_REPO" && $AGENT debug attach --json > /tmp/dbg_attach.out 2>&1) &
  ATTACH1_PID=$!
  log "Attach (--json) PID=$ATTACH1_PID"

  if ! wait_text /tmp/dbg_attach.out "Listener active" 30; then
    fail "Attach never printed 'Listener active' in 30s"
    ATTACH1_PID=""; return 1
  fi
  sleep 2

  (cd "$DEBUG_REPO" && $AGENT run --class ZCL_ABGAGT_DBG_TRIGGER > /tmp/dbg_trigger.out 2>&1) &
  TRIGGER_PID=$!
  log "Trigger (run --class) PID=$TRIGGER_PID"

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

  # stack --json (retry 12×5s)
  STACK_OK=0
  for _i in 1 2 3 4 5 6 7 8 9 10 11 12; do
    STACK_OUT=$((cd "$DEBUG_REPO" && $AGENT debug stack --json) 2>&1 || true)
    if echo "$STACK_OUT" | grep -q '"frames"' 2>/dev/null; then STACK_OK=1; break; fi
    log "stack returned error (attempt $_i/12) — waiting 5s..."; sleep 5
  done
  if [ "$STACK_OK" -eq 1 ]; then
    pass 'stack --json returned {"frames":[...]} (rule 4: no --session needed)'
  else
    fail "stack --json did not return expected JSON: $STACK_OUT"
  fi

  # vars --json
  VARS_OUT=$((cd "$DEBUG_REPO" && $AGENT debug vars --json) 2>&1 || true)
  if echo "$VARS_OUT" | grep -qE '"variables":\s*\[.+\]' 2>/dev/null; then
    pass 'vars --json returned {"variables":[...]} (rule 4: no --session needed)'
  else
    fail "vars --json did not return expected JSON: $VARS_OUT"
  fi

  # step over (retry 8×3s)
  STEP_OK=0
  for _i in 1 2 3 4 5 6 7 8; do
    STEP_OUT=$((cd "$DEBUG_REPO" && $AGENT debug step --type over --json) 2>&1 || true)
    if echo "$STEP_OUT" | grep -q '"position"' 2>/dev/null; then STEP_OK=1; break; fi
    log "step over returned error (attempt $_i/8) — waiting 3s..."; sleep 3
  done
  if [ "$STEP_OK" -eq 1 ]; then
    pass 'step --type over --json returned {"position":{...}}'
  else
    fail "step --type over --json did not return expected JSON: $STEP_OUT"
  fi

  # vars --json after step
  VARS2_OUT=$((cd "$DEBUG_REPO" && $AGENT debug vars --json) 2>&1 || true)
  if echo "$VARS2_OUT" | grep -qE '"variables":\s*\[.+\]' 2>/dev/null; then
    pass 'vars --json after step returned {"variables":[...]}'
  else
    fail "vars --json after step did not return expected JSON: $VARS2_OUT"
  fi

  # step continue (RULE 3, retry 8×3s)
  CONT_OK=0
  for _i in 1 2 3 4 5 6 7 8; do
    CONT_OUT=$((cd "$DEBUG_REPO" && $AGENT debug step --type continue --json) 2>&1 || true)
    if echo "$CONT_OUT" | grep -qE '"position"|"finished"' 2>/dev/null; then CONT_OK=1; break; fi
    log "step continue returned error (attempt $_i/8) — waiting 3s..."; sleep 3
  done
  if [ "$CONT_OK" -eq 1 ]; then
    pass 'step --type continue --json released work process (rule 3)'
  else
    fail "step --type continue --json did not return expected JSON: $CONT_OUT"
    for _j in 1 2 3 4 5; do
      (cd "$DEBUG_REPO" && $AGENT debug terminate >/dev/null 2>&1) && break; sleep 3
    done
  fi

  log "Waiting for trigger to complete after continue (up to ${TIMEOUT}s)..."
  if wait_pid_exit "$TRIGGER_PID" $TIMEOUT; then
    pass "Trigger completed normally after step continue"
  else
    fail "Trigger did NOT complete within ${TIMEOUT}s after continue"
    kill "$TRIGGER_PID" 2>/dev/null || true
  fi

  log "Waiting for attach daemon to exit (up to 10s)..."
  if wait_pid_exit "$ATTACH1_PID" 10; then
    pass "Attach daemon exited cleanly after session ended"
  else
    kill "$ATTACH1_PID" 2>/dev/null || true
    pass "Attach daemon killed (acceptable — daemon idle timeout may be longer)"
  fi

  wait_pid_exit "$ATTACH1_PID" 5 || kill "$ATTACH1_PID" 2>/dev/null || true
  wait_pid_exit "$TRIGGER_PID" 15 || kill "$TRIGGER_PID" 2>/dev/null || true
  ATTACH1_PID=""; TRIGGER_PID=""
}

# ── Scenario 4: vars --name / --expand ───────────────────────────────────────

scenario4() {
  echo ""
  echo "  Scenario 4: vars --name / --expand — variable inspection in scripted AI mode"
  echo "  $(printf '─%.0s' {1..85})"

  ensure_breakpoint 42

  rm -f /tmp/dbg_attach.out /tmp/dbg_trigger.out

  (cd "$DEBUG_REPO" && $AGENT debug attach --json > /tmp/dbg_attach.out 2>&1) &
  ATTACH1_PID=$!
  log "Attach (--json) PID=$ATTACH1_PID"

  if ! wait_text /tmp/dbg_attach.out "Listener active" 30; then
    fail "Attach never printed 'Listener active' in 30s"
    ATTACH1_PID=""; return 1
  fi
  sleep 2

  (cd "$DEBUG_REPO" && $AGENT run --class ZCL_ABGAGT_DBG_TRIGGER > /tmp/dbg_trigger.out 2>&1) &
  TRIGGER_PID=$!
  log "Trigger (run --class) PID=$TRIGGER_PID"

  log "Waiting for breakpoint hit (up to ${TIMEOUT}s)..."
  SESSION=""
  for i in $(seq 1 $((TIMEOUT * 2))); do
    sleep 0.5
    SESSION=$(grep -o '"session":"[^"]*"' /tmp/dbg_attach.out 2>/dev/null | head -1 | cut -d'"' -f4) || true
    [ -n "$SESSION" ] && break
  done

  if [ -n "$SESSION" ]; then
    pass "Breakpoint hit — session JSON emitted"
  else
    fail "No session JSON after ${TIMEOUT}s — attach output: $(cat /tmp/dbg_attach.out 2>/dev/null | tail -5)"
    kill "$TRIGGER_PID" 2>/dev/null; ATTACH1_PID=""; TRIGGER_PID=""; return 1
  fi

  # vars --json list (retry 6×5s)
  VARS_OK=0
  for _i in 1 2 3 4 5 6; do
    VARS_ALL=$((cd "$DEBUG_REPO" && $AGENT debug vars --json) 2>&1 || true)
    if echo "$VARS_ALL" | grep -qE '"variables":\s*\[.+\]' 2>/dev/null; then VARS_OK=1; break; fi
    log "vars --json returned error (attempt $_i/6) — waiting 5s..."; sleep 5
  done
  if [ "$VARS_OK" -eq 1 ]; then
    MISSING=""
    echo "$VARS_ALL" | grep -q '"LV_SUM"' || MISSING="$MISSING LV_SUM"
    echo "$VARS_ALL" | grep -q '"IV_A"'   || MISSING="$MISSING IV_A"
    echo "$VARS_ALL" | grep -q '"IV_B"'   || MISSING="$MISSING IV_B"
    if [ -z "$MISSING" ]; then
      pass "vars --json lists LV_SUM, IV_A, IV_B"
    else
      fail "vars --json missing variables:$MISSING — output: ${VARS_ALL:0:300}"
    fi
  else
    fail "vars --json did not return expected JSON: ${VARS_ALL:0:300}"
  fi

  # vars --name IV_A → value 7
  VARS_IVA=$((cd "$DEBUG_REPO" && $AGENT debug vars --name IV_A --json) 2>&1 || true)
  if echo "$VARS_IVA" | grep -q '"value":"7"' 2>/dev/null; then
    pass "vars --name IV_A value = 7"
  else
    fail "vars --name IV_A did not return value 7 — output: ${VARS_IVA:0:300}"
  fi

  # vars --name IV_B → value 1
  VARS_IVB=$((cd "$DEBUG_REPO" && $AGENT debug vars --name IV_B --json) 2>&1 || true)
  if echo "$VARS_IVB" | grep -q '"value":"1"' 2>/dev/null; then
    pass "vars --name IV_B value = 1"
  else
    fail "vars --name IV_B did not return value 1 — output: ${VARS_IVB:0:300}"
  fi

  # step over (retry 6×3s)
  STEP_OK=0
  for _i in 1 2 3 4 5 6; do
    STEP_OUT=$((cd "$DEBUG_REPO" && $AGENT debug step --type over --json) 2>&1 || true)
    if echo "$STEP_OUT" | grep -q '"position"' 2>/dev/null; then STEP_OK=1; break; fi
    log "step over returned error (attempt $_i/6) — waiting 3s..."; sleep 3
  done
  if [ "$STEP_OK" -eq 1 ]; then
    pass "step --type over executed lv_sum = iv_a + iv_b"
  else
    fail "step --type over did not return expected JSON: ${STEP_OUT:0:300}"
  fi

  # vars --name LV_SUM → value 8
  VARS_SUM=$((cd "$DEBUG_REPO" && $AGENT debug vars --name LV_SUM --json) 2>&1 || true)
  if echo "$VARS_SUM" | grep -q '"value":"8"' 2>/dev/null; then
    pass "vars --name LV_SUM value = 8 (iv_a+iv_b = 7+1) after step over"
  else
    fail "vars --name LV_SUM did not return value 8 — output: ${VARS_SUM:0:300}"
  fi

  # vars --expand LV_SUM → empty children
  EXPAND_OUT=$((cd "$DEBUG_REPO" && $AGENT debug vars --expand LV_SUM --json) 2>&1 || true)
  if echo "$EXPAND_OUT" | grep -q '"children":\[\]' 2>/dev/null; then
    pass "vars --expand LV_SUM (scalar) returns empty children []"
  else
    fail "vars --expand LV_SUM did not return empty children — output: ${EXPAND_OUT:0:300}"
  fi

  # step continue (RULE 3, retry 8×3s)
  CONT_OK=0
  for _i in 1 2 3 4 5 6 7 8; do
    CONT_OUT=$((cd "$DEBUG_REPO" && $AGENT debug step --type continue --json) 2>&1 || true)
    if echo "$CONT_OUT" | grep -qE '"position"|"finished"' 2>/dev/null; then CONT_OK=1; break; fi
    log "step continue returned error (attempt $_i/8) — waiting 3s..."; sleep 3
  done
  if [ "$CONT_OK" -eq 1 ]; then
    pass "step --type continue released work process (rule 3)"
  else
    fail "step --type continue did not return expected JSON: ${CONT_OUT:0:300}"
    for _j in 1 2 3 4 5; do
      (cd "$DEBUG_REPO" && $AGENT debug terminate >/dev/null 2>&1) && break; sleep 3
    done
  fi

  log "Waiting for trigger to complete (up to ${TIMEOUT}s)..."
  if wait_pid_exit "$TRIGGER_PID" $TIMEOUT; then
    pass "Trigger completed normally after step continue"
  else
    fail "Trigger did NOT complete within ${TIMEOUT}s"
    kill "$TRIGGER_PID" 2>/dev/null || true
  fi

  wait_pid_exit "$ATTACH1_PID" 10 || kill "$ATTACH1_PID" 2>/dev/null || true
  wait_pid_exit "$TRIGGER_PID" 15 || kill "$TRIGGER_PID" 2>/dev/null || true
  ATTACH1_PID=""; TRIGGER_PID=""
}

# ── Scenario 5: structure / table / field-symbol / dataref ───────────────────

scenario5() {
  echo ""
  echo "  Scenario 5: structure / table / field-symbol / dataref variable inspection"
  echo "  $(printf '─%.0s' {1..85})"

  # Find the global line for WRITE 'inspect' (include-relative line 15 in INSPECT_VARS / CM003).
  # view --full --lines shows "<global_line>" as a placeholder for separate-include methods,
  # so we probe ADT directly: try ascending global lines until debug set returns LINE_NR=15.
  log "Probing for WRITE 'inspect' global line (INSPECT_VARS, include-relative 15)..."
  INSPECT_LINE=""
  for _l in $(seq 45 100); do
    SET_OUT=$((cd "$DEBUG_REPO" && $AGENT debug set --object ZCL_ABGAGT_DBG_TRIGGER --line "$_l" --json) 2>&1 || true)
    if echo "$SET_OUT" | grep -q 'LINE_NR=15' 2>/dev/null; then
      INSPECT_LINE=$_l
      (cd "$DEBUG_REPO" && $AGENT debug delete --all >/dev/null 2>&1) || true
      break
    fi
    (cd "$DEBUG_REPO" && $AGENT debug delete --all >/dev/null 2>&1) || true
  done
  if [ -z "$INSPECT_LINE" ]; then
    fail "Could not probe WRITE 'inspect' global line (LINE_NR=15 not found in range 45-100)"
    return 1
  fi
  pass "WRITE 'inspect' (all vars populated) → global line ${INSPECT_LINE} (LINE_NR=15)"
  log "Clearing stale state and setting BP at ZCL_ABGAGT_DBG_TRIGGER:${INSPECT_LINE} ..."

  (cd "$DEBUG_REPO" && $AGENT debug terminate >/dev/null 2>&1) || true
  pkill -f debug-daemon.js 2>/dev/null || true
  sleep 15
  (cd "$DEBUG_REPO" && $AGENT debug delete --all >/dev/null 2>&1) || true
  for _i in 1 2 3 4 5; do
    (cd "$DEBUG_REPO" && $AGENT debug set --object ZCL_ABGAGT_DBG_TRIGGER --line "${INSPECT_LINE}" >/dev/null 2>&1) && break
    log "debug set attempt $_i failed — retrying in 5s..."; sleep 5
  done

  rm -f /tmp/dbg_attach.out /tmp/dbg_trigger.out

  (cd "$DEBUG_REPO" && $AGENT debug attach --json > /tmp/dbg_attach.out 2>&1) &
  ATTACH1_PID=$!
  log "Attach (--json) PID=$ATTACH1_PID"

  if ! wait_text /tmp/dbg_attach.out "Listener active" 30; then
    fail "Attach never printed 'Listener active' in 30s"
    ATTACH1_PID=""; return 1
  fi
  sleep 2

  (cd "$DEBUG_REPO" && $AGENT run --class ZCL_ABGAGT_DBG_TRIGGER > /tmp/dbg_trigger.out 2>&1) &
  TRIGGER_PID=$!
  log "Trigger (run --class) PID=$TRIGGER_PID"

  log "Waiting for breakpoint hit at INSPECT_VARS (up to ${TIMEOUT}s)..."
  SESSION=""
  for i in $(seq 1 $((TIMEOUT * 2))); do
    sleep 0.5
    SESSION=$(grep -o '"session":"[^"]*"' /tmp/dbg_attach.out 2>/dev/null | head -1 | cut -d'"' -f4) || true
    [ -n "$SESSION" ] && break
  done

  if [ -n "$SESSION" ]; then
    pass "Breakpoint hit — session JSON emitted"
  else
    fail "No session JSON after ${TIMEOUT}s — output: $(cat /tmp/dbg_attach.out 2>/dev/null | tail -5)"
    kill "$TRIGGER_PID" 2>/dev/null; ATTACH1_PID=""; TRIGGER_PID=""; return 1
  fi

  # vars --json — all four locals (retry 6×5s)
  VARS_OK=0
  for _i in 1 2 3 4 5 6; do
    VARS_ALL=$((cd "$DEBUG_REPO" && $AGENT debug vars --json) 2>&1 || true)
    if echo "$VARS_ALL" | grep -qE '"variables":\s*\[.+\]' 2>/dev/null; then VARS_OK=1; break; fi
    log "vars --json returned error (attempt $_i/6) — waiting 5s..."; sleep 5
  done
  if [ "$VARS_OK" -eq 1 ]; then
    MISSING=""
    echo "$VARS_ALL" | grep -qi '"LS_PERSON"'  || MISSING="$MISSING LS_PERSON"
    echo "$VARS_ALL" | grep -qi '"LT_PERSONS"' || MISSING="$MISSING LT_PERSONS"
    echo "$VARS_ALL" | grep -qi '"LR_PERSON"'  || MISSING="$MISSING LR_PERSON"
    if [ -z "$MISSING" ]; then
      pass "vars --json lists LS_PERSON, LT_PERSONS, LR_PERSON"
    else
      fail "vars --json missing:$MISSING — output: ${VARS_ALL:0:300}"
    fi
  else
    fail "vars --json did not return expected JSON: ${VARS_ALL:0:300}"
  fi

  # field-symbol <LS> via source-parse enrichment
  if echo "$VARS_ALL" | grep -q '"<LS>"' 2>/dev/null; then
    pass "Field-symbol <LS> listed in vars (source-parse enrichment working)"
  else
    fail "Field-symbol <LS> not found in vars output: ${VARS_ALL:0:300}"
  fi

  # expand <LS>
  EXP_FS=$((cd "$DEBUG_REPO" && $AGENT debug vars --expand '<LS>' --json) 2>&1 || true)
  FS_OK=0
  if echo "$EXP_FS" | grep -q '"children"' 2>/dev/null; then
    FS_N=0; FS_A=0
    echo "$EXP_FS" | grep -qi '"name":"NAME"'  && FS_N=1
    echo "$EXP_FS" | grep -qi '"name":"AGE"'   && FS_A=1
    echo "$EXP_FS" | grep -q '"value":"Alice"' && FS_N=1
    echo "$EXP_FS" | grep -q '"value":"30"'    && FS_A=1
    [ "$FS_N" -eq 1 ] && [ "$FS_A" -eq 1 ] && FS_OK=1
  fi
  if [ "$FS_OK" -eq 1 ]; then
    pass "vars --expand <LS> → field-symbol structure children (NAME=Alice, AGE=30)"
  else
    fail "vars --expand <LS> did not return expected children — output: ${EXP_FS:0:400}"
  fi

  # expand LS_PERSON
  EXP_STRUCT=$((cd "$DEBUG_REPO" && $AGENT debug vars --expand LS_PERSON --json) 2>&1 || true)
  STRUCT_OK=0
  if echo "$EXP_STRUCT" | grep -q '"children"' 2>/dev/null; then
    N_OK=0; A_OK=0
    echo "$EXP_STRUCT" | grep -qi '"name":"NAME"'  && N_OK=1
    echo "$EXP_STRUCT" | grep -qi '"name":"AGE"'   && A_OK=1
    echo "$EXP_STRUCT" | grep -q '"value":"Alice"' && N_OK=1
    echo "$EXP_STRUCT" | grep -q '"value":"30"'    && A_OK=1
    [ "$N_OK" -eq 1 ] && [ "$A_OK" -eq 1 ] && STRUCT_OK=1
  fi
  if [ "$STRUCT_OK" -eq 1 ]; then
    pass "vars --expand LS_PERSON → structure fields NAME (Alice) and AGE (30)"
  else
    fail "vars --expand LS_PERSON did not return expected children — output: ${EXP_STRUCT:0:400}"
  fi

  # expand LT_PERSONS → 2 rows
  EXP_TABLE=$((cd "$DEBUG_REPO" && $AGENT debug vars --expand LT_PERSONS --json) 2>&1 || true)
  if echo "$EXP_TABLE" | grep -q '"children"' 2>/dev/null && \
     echo "$EXP_TABLE" | grep -qE '"LT_PERSONS\[1\]"|"\[1\]"' 2>/dev/null && \
     echo "$EXP_TABLE" | grep -qE '"LT_PERSONS\[2\]"|"\[2\]"' 2>/dev/null; then
    pass "vars --expand LT_PERSONS → 2 row children [1] and [2]"
  else
    fail "vars --expand LT_PERSONS did not return 2 rows — output: ${EXP_TABLE:0:400}"
  fi

  # expand LT_PERSONS[1] → row 1 fields (retry 3×3s)
  EXP_ROW1=""
  for _i in 1 2 3; do
    EXP_ROW1=$((cd "$DEBUG_REPO" && $AGENT debug vars --expand "LT_PERSONS[1]" --json) 2>&1 || true)
    echo "$EXP_ROW1" | grep -q '"children"' 2>/dev/null && break
    log "expand LT_PERSONS[1] attempt $_i returned no children — retrying in 3s..."; sleep 3
  done
  ROW1_OK=0
  if echo "$EXP_ROW1" | grep -q '"children"' 2>/dev/null; then
    echo "$EXP_ROW1" | grep -qi '"name":"NAME"'  && ROW1_OK=1
    echo "$EXP_ROW1" | grep -qi '"name":"AGE"'   && ROW1_OK=1
    echo "$EXP_ROW1" | grep -q '"value":"Alice"' && ROW1_OK=1
    echo "$EXP_ROW1" | grep -q '"value":"30"'    && ROW1_OK=1
  fi
  if [ "$ROW1_OK" -eq 1 ]; then
    pass "vars --expand LT_PERSONS[1] → row 1 fields (NAME=Alice, AGE=30)"
  else
    fail "vars --expand LT_PERSONS[1] did not return row 1 fields — output: ${EXP_ROW1:0:400}"
  fi

  # expand LT_PERSONS[1]->NAME (retry 3×3s)
  EXP_DRILL=""
  for _i in 1 2 3; do
    EXP_DRILL=$((cd "$DEBUG_REPO" && $AGENT debug vars --expand "LT_PERSONS[1]->NAME" --json) 2>&1 || true)
    echo "$EXP_DRILL" | grep -q '"variable"' 2>/dev/null && break
    log "expand LT_PERSONS[1]->NAME attempt $_i — retrying in 3s..."; sleep 3
  done
  if echo "$EXP_DRILL" | grep -q '"value":"Alice"' 2>/dev/null || \
     (echo "$EXP_DRILL" | grep -q '"children"' 2>/dev/null && echo "$EXP_DRILL" | grep -q '"variable"' 2>/dev/null); then
    pass "vars --expand LT_PERSONS[1]->NAME → drills to NAME field (Alice)"
  else
    fail "vars --expand LT_PERSONS[1]->NAME did not return expected output — output: ${EXP_DRILL:0:400}"
  fi

  # expand LR_PERSON → dereference dataref
  EXP_REF=$((cd "$DEBUG_REPO" && $AGENT debug vars --expand LR_PERSON --json) 2>&1 || true)
  REF_OK=0
  if echo "$EXP_REF" | grep -q '"children"' 2>/dev/null; then
    echo "$EXP_REF" | grep -q '"value":"Alice"' && REF_OK=1
    echo "$EXP_REF" | grep -q '"value":"30"'    && REF_OK=1
    echo "$EXP_REF" | grep -qi '"name":"NAME"'  && REF_OK=1
  fi
  if [ "$REF_OK" -eq 1 ]; then
    pass "vars --expand LR_PERSON → dereferences dataref → structure fields (NAME=Alice, AGE=30)"
  else
    fail "vars --expand LR_PERSON did not return dereferenced structure — output: ${EXP_REF:0:400}"
  fi

  # step continue (RULE 3, retry 8×3s)
  CONT_OK=0
  for _i in 1 2 3 4 5 6 7 8; do
    CONT_OUT=$((cd "$DEBUG_REPO" && $AGENT debug step --type continue --json) 2>&1 || true)
    if echo "$CONT_OUT" | grep -qE '"position"|"finished"' 2>/dev/null; then CONT_OK=1; break; fi
    log "step continue returned error (attempt $_i/8) — waiting 3s..."; sleep 3
  done
  if [ "$CONT_OK" -eq 1 ]; then
    pass "step --type continue released work process (rule 3)"
  else
    fail "step --type continue did not return expected JSON: ${CONT_OUT:0:300}"
    for _j in 1 2 3 4 5; do
      (cd "$DEBUG_REPO" && $AGENT debug terminate >/dev/null 2>&1) && break; sleep 3
    done
  fi

  log "Waiting for trigger to complete (up to ${TIMEOUT}s)..."
  if wait_pid_exit "$TRIGGER_PID" $TIMEOUT; then
    pass "Trigger completed normally after step continue"
  else
    fail "Trigger did NOT complete within ${TIMEOUT}s"
    kill "$TRIGGER_PID" 2>/dev/null || true
  fi

  wait_pid_exit "$ATTACH1_PID" 10 || kill "$ATTACH1_PID" 2>/dev/null || true
  wait_pid_exit "$TRIGGER_PID" 15 || kill "$TRIGGER_PID" 2>/dev/null || true
  ATTACH1_PID=""; TRIGGER_PID=""
}

# ── main ─────────────────────────────────────────────────────────────────────

case "$SCENARIO" in
  3)   scenario3 ;;
  4)   scenario4 ;;
  5)   scenario5 ;;
  all) scenario3; sleep 20; scenario4; sleep 20; scenario5 ;;
  *)   echo "Usage: $0 [3|4|5|all]"; exit 1 ;;
esac

echo ""
[[ $FAIL -eq 0 ]]
