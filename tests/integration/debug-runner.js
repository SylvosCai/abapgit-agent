/**
 * Debug Command Test Runner
 *
 * Tests ABAP debugger — breakpoint management across all object types.
 * Requires the abgagt-debug-test repo checked out at ../abgagt-debug-test
 * (sibling of the abapgit-agent repo).
 *
 * Objects used:
 *   ZCL_ABGAGT_DBG_TRIGGER — trigger class (CLAS main, testclasses, locals_imp)
 *   LZABGAGT_DBG_TESTU01   — FUGR include of ZABGAGT_DBG_TEST
 *
 * Group A (13 tests): Hardcoded known-good lines — verify ADT accepts them and
 *   returns the expected LINE_NR (include-relative) and line (global) values.
 *
 * Group B (24 tests): view --full --lines → parse hint → debug set → debug list
 *   round-trip for CLAS main, testclasses, locals_imp, FUGR include, CLAS main
 *   with active ENHO (verifies countEnhoLines correction gives base-source line),
 *   and non-ENHO method in the same ENHO-active class (verifies no hint shift).
 *   Catches regressions where view --lines emits wrong line numbers.
 *
 * Group C (8 tests): Unexecutable line rejection — comments, blank lines, DATA
 *   declarations, METHOD statements, and ENHO-injected G numbers.
 *   ADT must return stale JSON.
 *
 * Confirmed ADT-accepted lines (probed via direct POST, 2026-04-22):
 *   ZCL_ABGAGT_DBG_TRIGGER main:        line 42, LINE_NR=5  (lv_sum = iv_a + iv_b)
 *   ZCL_ABGAGT_DBG_TRIGGER testclasses: line 13, LINE_NR=13 (rv_val = iv_val * lv_two)
 *   ZCL_ABGAGT_DBG_TRIGGER locals_imp:  line 6,  LINE_NR=6  (lv_label = 'debug-test')
 *   LZABGAGT_DBG_TESTU01 (FUGR):        line 13, LINE_NR=13 (lv_result = iv_a + iv_b)
 */

'use strict';

const { execSync } = require('child_process');
const path = require('path');

// NOTE: LINE_NR is include-relative, not the global assembled-source line.
// For CLAS main methods it differs from the line we passed in (e.g. global 42 → LINE_NR 5).
function adtLineNr(jsonOutput) {
  const m = jsonOutput.match(/"id":"[^"]*LINE_NR=(\d+)/);
  return m ? parseInt(m[1], 10) : null;
}

// Helper: parse the "line" field from --json output (echoes the line we passed in).
function jsonLine(jsonOutput) {
  const m = jsonOutput.match(/"line":(\d+)/);
  return m ? parseInt(m[1], 10) : null;
}

// Parse a `debug set --objects NAME:N` hint from view --lines output.
// The hint appears in lines like:
//   * ---- Method: COMPUTE (CM001) — breakpoint: debug set --objects ZCL_ABGAGT_DBG_TRIGGER:42 ----
//   * ---- FM: ZABGAGT_DBG_ADD (LZABGAGT_DBG_TESTU01) — breakpoint: debug set --objects LZABGAGT_DBG_TESTU01:13 ----
//
// For CLAS main methods the source lines use G [N] format:
//     42 [  5]      lv_sum = iv_a + iv_b.
// where G=42 is the global assembled-source line (sent to ADT as #start=)
// and N=5 is the include-relative line (stored by ADT as LINE_NR).
// For sub-includes (testclasses, locals_imp) and FUGR the lines show just N = LINE_NR.
//
// Returns { object, line, lineNr, include } or null if not found.
//   line   — global assembled-source line (what we pass to `debug set`)
//   lineNr — include-relative line (what ADT returns as LINE_NR in the id)
function parseViewHint(viewOutput, methodName) {
  const re = new RegExp(
    `---- (?:Method|FM): ${methodName}[^\\n]*breakpoint: debug set --objects ([A-Z0-9_]+):(\\d+)(?:\\s+--include\\s+(\\S+))?`,
    'i'
  );
  const m = viewOutput.match(re);
  if (!m) return null;
  const line = parseInt(m[2], 10);
  const include = m[3] || null;

  // For CLAS main (no --include, has G [N] lines): scan the lines after the
  // hint to find the source line matching the global line number, extract [N].
  let lineNr = line; // default: section line = LINE_NR (testclasses/locals_imp/FUGR)
  if (!include) {
    const afterHint = viewOutput.slice(viewOutput.indexOf(m[0]) + m[0].length);
    const lineRe = new RegExp(`^\\s+${line}\\s+\\[\\s*(\\d+)\\]`, 'm');
    const lm = afterHint.match(lineRe);
    if (lm) lineNr = parseInt(lm[1], 10);
  }

  return { object: m[1], line, lineNr, include };
}

function buildTestCases() {
  // Shared state for Group B — view output is parsed in verify() and consumed
  // by the next test's args() / verify(). Tests run sequentially so this is safe.
  const viewParsed = {
    clasMain: null,
    testclasses: null,
    localsImp: null,
    fugr: null,
    clasMainEnho: null,      // B5: CLAS main with active ENHO (COMPUTE method)
    clasMainNoEnho: null,    // B5b: non-ENHO method in same ENHO-active class (MAIN)
  };

  return [
    // =================================================================
    // GROUP A: Hardcoded known-good lines — verify ADT URI correctness
    // =================================================================

    {
      name: 'debug delete --all (setup: clear any stale breakpoints)',
      args: ['delete', '--all'],
      expectSuccess: true,
      verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
    },

    // --- CLAS main source breakpoint ---
    {
      name: '[A] debug set CLAS main — line 42 accepted by ADT (LINE_NR=5)',
      // Global line 42 = CM001 include line 5 (as shown by view --lines: "42 [  5]")
      args: ['set', '--object', 'ZCL_ABGAGT_DBG_TRIGGER', '--line', '42', '--json'],
      expectSuccess: true,
      verify: (output) => adtLineNr(output) === 5 && jsonLine(output) === 42
    },
    {
      name: '[A] debug list shows CLAS main breakpoint',
      args: ['list'],
      expectSuccess: true,
      verify: (output) => output.includes('ZCL_ABGAGT_DBG_TRIGGER') && output.includes('42')
    },
    {
      name: '[A] debug delete --all (cleanup after CLAS main test)',
      args: ['delete', '--all'],
      expectSuccess: true,
      verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
    },
    {
      name: '[A] debug list shows no breakpoints after delete',
      args: ['list'],
      expectSuccess: true,
      verify: (output) => !output.includes('ZCL_ABGAGT_DBG_TRIGGER')
    },

    // --- testclasses include breakpoint ---
    {
      name: '[A] debug set testclasses — line 13 accepted by ADT (LINE_NR=13)',
      // (skips comment on line 10, DATA on line 11, blank on line 12)
      args: ['set', '--objects', 'ZCL_ABGAGT_DBG_TRIGGER:13', '--include', 'testclasses', '--json'],
      expectSuccess: true,
      verify: (output) => adtLineNr(output) === 13 && jsonLine(output) === 13
    },
    {
      name: '[A] debug list shows testclasses breakpoint',
      args: ['list'],
      expectSuccess: true,
      verify: (output) => output.includes('ZCL_ABGAGT_DBG_TRIGGER') && output.includes('13')
    },

    // --- locals_imp include breakpoint ---
    {
      name: '[A] debug set locals_imp — line 6 accepted by ADT (LINE_NR=6)',
      // (skips comment on line 3, DATA on line 4, blank on line 5)
      args: ['set', '--objects', 'ZCL_ABGAGT_DBG_TRIGGER:6', '--include', 'locals_imp', '--json'],
      expectSuccess: true,
      verify: (output) => adtLineNr(output) === 6 && jsonLine(output) === 6
    },
    {
      name: '[A] debug list shows both testclasses and locals_imp breakpoints',
      args: ['list'],
      expectSuccess: true,
      verify: (output) =>
        output.includes('ZCL_ABGAGT_DBG_TRIGGER') &&
        output.includes('13') &&
        output.includes('6')
    },
    {
      name: '[A] debug delete --all (cleanup after sub-include tests)',
      args: ['delete', '--all'],
      expectSuccess: true,
      verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
    },

    // --- FUGR include breakpoint (regression: wrong ADT URI /programs/includes/ before fix) ---
    // LZABGAGT_DBG_TESTU01 is the FM source include of ZABGAGT_DBG_TEST.
    // Before the fix, this was routed to /programs/includes/ (wrong path).
    // Correct path: /functions/groups/zcais_dbg_test/includes/lzcais_dbg_testu01/source/main
    {
      name: '[A] debug set FUGR include — line 13 accepted by ADT (LINE_NR=13)',
      args: ['set', '--objects', 'LZABGAGT_DBG_TESTU01:13', '--json'],
      expectSuccess: true,
      verify: (output) => adtLineNr(output) === 13 && jsonLine(output) === 13
    },
    {
      name: '[A] debug list shows FUGR include breakpoint',
      args: ['list'],
      expectSuccess: true,
      verify: (output) => output.includes('LZABGAGT_DBG_TESTU01') && output.includes('13')
    },
    {
      name: '[A] debug delete --all (cleanup after FUGR test)',
      args: ['delete', '--all'],
      expectSuccess: true,
      verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
    },

    // =================================================================
    // GROUP B: view --lines → parse hint → set → verify round-trip
    // Catches regressions where view --lines emits wrong line numbers.
    // Each chain: view (parse+store) → set (use stored line) → list (verify)
    // =================================================================

    // B1: CLAS main method (COMPUTE)
    {
      name: '[B] view --full --lines CLAS: parse COMPUTE method hint',
      run: 'view',
      args: ['--objects', 'ZCL_ABGAGT_DBG_TRIGGER', '--full', '--lines'],
      expectSuccess: true,
      verify: (output) => {
        const hint = parseViewHint(output, 'COMPUTE');
        if (!hint) return false;
        viewParsed.clasMain = hint;
        return hint.object === 'ZCL_ABGAGT_DBG_TRIGGER' &&
          hint.line >= 1 && hint.line <= 999 &&
          hint.include === null;
      }
    },
    {
      name: '[B] debug set CLAS main at view-derived line — LINE_NR matches [N]',
      get args() {
        const h = viewParsed.clasMain;
        return h ? ['set', '--objects', `${h.object}:${h.line}`, '--json'] : ['--help'];
      },
      expectSuccess: true,
      verify: (output) => {
        const h = viewParsed.clasMain;
        // h.lineNr is the [N] value from "G [N]" in view --lines = ADT LINE_NR
        return h !== null && adtLineNr(output) === h.lineNr && jsonLine(output) === h.line;
      }
    },
    {
      name: '[B] debug list confirms CLAS main BP at view-derived line',
      args: ['list'],
      expectSuccess: true,
      verify: (output) => {
        const h = viewParsed.clasMain;
        return output.includes('ZCL_ABGAGT_DBG_TRIGGER') && h !== null && output.includes(String(h.line));
      }
    },
    {
      name: '[B] debug delete --all (cleanup after B1)',
      args: ['delete', '--all'],
      expectSuccess: true,
      verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
    },

    // B2: CLAS testclasses (DOUBLE in ltcl_helper)
    {
      name: '[B] view --full --lines CLAS: parse DOUBLE (testclasses) hint',
      run: 'view',
      args: ['--objects', 'ZCL_ABGAGT_DBG_TRIGGER', '--full', '--lines'],
      expectSuccess: true,
      verify: (output) => {
        const hint = parseViewHint(output, 'DOUBLE');
        if (!hint) return false;
        viewParsed.testclasses = hint;
        return hint.object === 'ZCL_ABGAGT_DBG_TRIGGER' &&
          hint.line >= 1 && hint.line <= 999 &&
          hint.include === 'testclasses';
      }
    },
    {
      name: '[B] debug set testclasses at view-derived line — LINE_NR=line matches',
      get args() {
        const h = viewParsed.testclasses;
        return h
          ? ['set', '--objects', `${h.object}:${h.line}`, '--include', h.include, '--json']
          : ['--help'];
      },
      expectSuccess: true,
      verify: (output) => {
        const h = viewParsed.testclasses;
        return h !== null && adtLineNr(output) === h.lineNr && jsonLine(output) === h.line;
      }
    },
    {
      name: '[B] debug list confirms testclasses BP at view-derived line',
      args: ['list'],
      expectSuccess: true,
      verify: (output) => {
        const h = viewParsed.testclasses;
        return output.includes('ZCL_ABGAGT_DBG_TRIGGER') && h !== null && output.includes(String(h.line));
      }
    },
    {
      name: '[B] debug delete --all (cleanup after B2)',
      args: ['delete', '--all'],
      expectSuccess: true,
      verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
    },

    // B3: CLAS locals_imp (DESCRIBE in lcl_helper)
    {
      name: '[B] view --full --lines CLAS: parse DESCRIBE (locals_imp) hint',
      run: 'view',
      args: ['--objects', 'ZCL_ABGAGT_DBG_TRIGGER', '--full', '--lines'],
      expectSuccess: true,
      verify: (output) => {
        const hint = parseViewHint(output, 'DESCRIBE');
        if (!hint) return false;
        viewParsed.localsImp = hint;
        return hint.object === 'ZCL_ABGAGT_DBG_TRIGGER' &&
          hint.line >= 1 && hint.line <= 999 &&
          hint.include === 'locals_imp';
      }
    },
    {
      name: '[B] debug set locals_imp at view-derived line — LINE_NR=line matches',
      get args() {
        const h = viewParsed.localsImp;
        return h
          ? ['set', '--objects', `${h.object}:${h.line}`, '--include', h.include, '--json']
          : ['--help'];
      },
      expectSuccess: true,
      verify: (output) => {
        const h = viewParsed.localsImp;
        return h !== null && adtLineNr(output) === h.lineNr && jsonLine(output) === h.line;
      }
    },
    {
      name: '[B] debug list confirms locals_imp BP at view-derived line',
      args: ['list'],
      expectSuccess: true,
      verify: (output) => {
        const h = viewParsed.localsImp;
        return output.includes('ZCL_ABGAGT_DBG_TRIGGER') && h !== null && output.includes(String(h.line));
      }
    },
    {
      name: '[B] debug delete --all (cleanup after B3)',
      args: ['delete', '--all'],
      expectSuccess: true,
      verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
    },

    // B4: FUGR FM (ZABGAGT_DBG_ADD)
    {
      name: '[B] view --full --lines FUGR: parse ZABGAGT_DBG_ADD FM hint',
      run: 'view',
      args: ['--objects', 'ZABGAGT_DBG_TEST', '--type', 'FUGR', '--full', '--fm', 'ZABGAGT_DBG_ADD', '--lines'],
      expectSuccess: true,
      verify: (output) => {
        const hint = parseViewHint(output, 'ZABGAGT_DBG_ADD');
        if (!hint) return false;
        viewParsed.fugr = hint;
        return hint.object === 'LZABGAGT_DBG_TESTU01' &&
          hint.line >= 1 && hint.line <= 999 &&
          hint.include === null;
      }
    },
    {
      name: '[B] debug set FUGR at view-derived line — LINE_NR=line matches',
      get args() {
        const h = viewParsed.fugr;
        return h
          ? ['set', '--objects', `${h.object}:${h.line}`, '--json']
          : ['--help'];
      },
      expectSuccess: true,
      verify: (output) => {
        const h = viewParsed.fugr;
        return h !== null && adtLineNr(output) === h.lineNr && jsonLine(output) === h.line;
      }
    },
    {
      name: '[B] debug list confirms FUGR BP at view-derived line',
      args: ['list'],
      expectSuccess: true,
      verify: (output) => {
        const h = viewParsed.fugr;
        return output.includes('LZABGAGT_DBG_TESTU01') && h !== null && output.includes(String(h.line));
      }
    },
    {
      name: '[B] debug delete --all (final cleanup after B4)',
      args: ['delete', '--all'],
      expectSuccess: true,
      verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
    },

    // B5: CLAS main with active ENHO (ZABGAGT_DBG_ENHO hooks into COMPUTE at BEGIN)
    // view --full --lines must show the injected ENHO block and emit a BP hint that
    // uses base-source coordinates (countEnhoLines subtraction), not runtime G numbers.
    // With ENHO active: METHOD compute. at G=38, ENHO injects 4 lines (G 39-42),
    // lv_sum = iv_a + iv_b. is at G=46 in the display but the hint must say :42
    // (base-source line that ADT validates against). LINE_NR=5 (include-relative).
    {
      name: '[B5] view --full --lines CLAS with ENHO: parse COMPUTE hint (expect base-source :42)',
      run: 'view',
      args: ['--objects', 'ZCL_ABGAGT_DBG_TRIGGER', '--full', '--lines'],
      expectSuccess: true,
      verify: (output) => {
        // Must contain the injected ENHO block marker
        if (!output.includes('*"* ENHO: ZABGAGT_DBG_ENHO')) return false;
        const hint = parseViewHint(output, 'COMPUTE');
        if (!hint) return false;
        viewParsed.clasMainEnho = hint;
        // Hint must be base-source line 42, not runtime G=46
        return hint.object === 'ZCL_ABGAGT_DBG_TRIGGER' &&
          hint.line === 42 &&
          hint.include === null;
      }
    },
    {
      name: '[B5] debug set CLAS+ENHO at hint line 42 — ADT accepts (LINE_NR=5)',
      get args() {
        const h = viewParsed.clasMainEnho;
        return h ? ['set', '--objects', `${h.object}:${h.line}`, '--json'] : ['--help'];
      },
      expectSuccess: true,
      verify: (output) => {
        const h = viewParsed.clasMainEnho;
        return h !== null && adtLineNr(output) === h.lineNr && jsonLine(output) === h.line;
      }
    },
    {
      name: '[B5] debug list confirms CLAS+ENHO BP at line 42',
      args: ['list'],
      expectSuccess: true,
      verify: (output) => {
        const h = viewParsed.clasMainEnho;
        return output.includes('ZCL_ABGAGT_DBG_TRIGGER') && h !== null && output.includes(String(h.line));
      }
    },
    {
      name: '[B5] debug delete --all (cleanup after B5)',
      args: ['delete', '--all'],
      expectSuccess: true,
      verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
    },

    // B5b: non-ENHO method (IF_OO_ADT_CLASSRUN~MAIN, CM002) in the same ENHO-active class.
    // ENHO only injects into COMPUTE; MAIN must be unaffected — its G numbers and hint
    // must NOT be shifted by the ENHO lines that appear in a preceding CM section.
    {
      name: '[B5b] view --full --lines CLAS+ENHO: parse IF_OO_ADT_CLASSRUN~MAIN hint (no shift)',
      run: 'view',
      args: ['--objects', 'ZCL_ABGAGT_DBG_TRIGGER', '--full', '--lines'],
      expectSuccess: true,
      verify: (output) => {
        // MAIN must appear and the hint must be ADT-accepted (LINE_NR=4 = [4] in output)
        const hint = parseViewHint(output, 'IF_OO_ADT_CLASSRUN~MAIN');
        if (!hint) return false;
        viewParsed.clasMainNoEnho = hint;
        return hint.object === 'ZCL_ABGAGT_DBG_TRIGGER' &&
          hint.line >= 1 && hint.line <= 999 &&
          hint.include === null;
      }
    },
    {
      name: '[B5b] debug set CLAS+ENHO non-ENHO method at hint line — ADT accepts (LINE_NR=4)',
      get args() {
        const h = viewParsed.clasMainNoEnho;
        return h ? ['set', '--objects', `${h.object}:${h.line}`, '--json'] : ['--help'];
      },
      expectSuccess: true,
      verify: (output) => {
        const h = viewParsed.clasMainNoEnho;
        return h !== null && adtLineNr(output) === h.lineNr && jsonLine(output) === h.line;
      }
    },
    {
      name: '[B5b] debug list confirms non-ENHO method BP',
      args: ['list'],
      expectSuccess: true,
      verify: (output) => {
        const h = viewParsed.clasMainNoEnho;
        return output.includes('ZCL_ABGAGT_DBG_TRIGGER') && h !== null && output.includes(String(h.line));
      }
    },
    {
      name: '[B5b] debug delete --all (cleanup after B5b)',
      args: ['delete', '--all'],
      expectSuccess: true,
      verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
    },

    // =================================================================
    // GROUP C: Unexecutable lines — ADT must reject with error JSON
    //
    // Unexecutable lines used (verified against ADT 2026-04-02):
    //   CLAS main line 16: DATA lv_result TYPE i.  (variable declaration, in main)
    //   CLAS main line 29: METHOD compute.         (method statement)
    //   testclasses line 9: METHOD double.         (method statement)
    //   locals_imp  line 2: METHOD describe.       (method statement)
    //   FUGR        line 2: *"-----...             (comment)
    //   FUGR        line 12: (blank line)
    // =================================================================

    {
      name: '[C] debug set CLAS main line 16 (DATA decl) — rejected by ADT',
      args: ['set', '--objects', 'ZCL_ABGAGT_DBG_TRIGGER:16', '--json'],
      expectSuccess: false,
      verify: (output) =>
        output.includes('"error"') && output.includes('"stale"') &&
        output.includes('"line":16') && !output.includes('"id"')
    },
    {
      name: '[C] debug set CLAS main line 29 (METHOD stmt) — rejected by ADT',
      args: ['set', '--objects', 'ZCL_ABGAGT_DBG_TRIGGER:29', '--json'],
      expectSuccess: false,
      verify: (output) =>
        output.includes('"error"') && output.includes('"stale"') && output.includes('"line":29')
    },
    {
      name: '[C] debug set testclasses line 9 (METHOD stmt) — rejected by ADT',
      args: ['set', '--objects', 'ZCL_ABGAGT_DBG_TRIGGER:9', '--include', 'testclasses', '--json'],
      expectSuccess: false,
      verify: (output) =>
        output.includes('"error"') && output.includes('"stale"') && output.includes('"line":9')
    },
    {
      name: '[C] debug set locals_imp line 2 (METHOD stmt) — rejected by ADT',
      args: ['set', '--objects', 'ZCL_ABGAGT_DBG_TRIGGER:2', '--include', 'locals_imp', '--json'],
      expectSuccess: false,
      verify: (output) =>
        output.includes('"error"') && output.includes('"stale"') && output.includes('"line":2')
    },
    {
      name: '[C] debug set FUGR line 2 (comment) — rejected by ADT',
      args: ['set', '--objects', 'LZABGAGT_DBG_TESTU01:2', '--json'],
      expectSuccess: false,
      verify: (output) =>
        output.includes('"error"') && output.includes('"stale"') && output.includes('"line":2')
    },
    {
      name: '[C] debug set FUGR line 12 (blank line) — rejected by ADT',
      args: ['set', '--objects', 'LZABGAGT_DBG_TESTU01:12', '--json'],
      expectSuccess: false,
      verify: (output) =>
        output.includes('"error"') && output.includes('"stale"') && output.includes('"line":12')
    },

    // ENHO-injected lines in view --full --lines show G numbers 30-32 for the hook body.
    // These G numbers do NOT exist in ADT's base-source validation → rejected.
    {
      name: '[C] debug set CLAS+ENHO line 30 (injected ENHO marker) — rejected by ADT',
      args: ['set', '--objects', 'ZCL_ABGAGT_DBG_TRIGGER:30', '--json'],
      expectSuccess: false,
      verify: (output) =>
        output.includes('"error"') && output.includes('"stale"') && output.includes('"line":30')
    },
    {
      name: '[C] debug set CLAS+ENHO line 32 (injected ENHO body) — rejected by ADT',
      args: ['set', '--objects', 'ZCL_ABGAGT_DBG_TRIGGER:32', '--json'],
      expectSuccess: false,
      verify: (output) =>
        output.includes('"error"') && output.includes('"stale"') && output.includes('"line":32')
    },
  ];
}

/**
 * Run debug integration tests.
 * @param {string} repoRoot    - abapgit-agent repo root (for bin/abapgit-agent)
 * @param {object} helpers     - { printSubHeader, printSuccess, printError, printWarning, printInfo, colorize, colors }
 */
function runDebugTests(repoRoot, helpers) {
  const { printSubHeader, printSuccess, printError, colorize, colors } = helpers;

  printSubHeader('Running Debug Tests (breakpoint management)');

  const agentBin = path.join(repoRoot, 'bin', 'abapgit-agent');

  // Ensure breakpoints are always deleted on exit (normal, crash, or signal).
  // Without this, a mid-test crash leaves stale breakpoints that cause subsequent
  // runs to fail with "Breakpoint already exists" or unexpected session hits.
  const deleteAllOnExit = () => {
    try { execSync(`node ${agentBin} debug delete --all`, { cwd: repoRoot, timeout: 15000 }); } catch { /* best-effort */ }
  };
  process.once('exit',    deleteAllOnExit);
  process.once('SIGINT',  () => { deleteAllOnExit(); process.exit(130); });
  process.once('SIGTERM', () => { deleteAllOnExit(); process.exit(143); });

  const testCases = buildTestCases();
  const startTime = Date.now();
  const results = [];
  const failed = [];

  for (const testCase of testCases) {
    process.stdout.write(`  Testing: debug ${testCase.name}... `);

    let passed = false;
    let output = '';

    try {
      const subcommand = testCase.run || 'debug';
      const args = [subcommand, ...testCase.args];
      output = execSync(
        `node ${agentBin} ${args.join(' ')}`,
        { cwd: repoRoot, encoding: 'utf8', timeout: 120000 }
      );
      passed = testCase.verify ? testCase.verify(output) : true;
    } catch (error) {
      output = error.stdout || error.stderr || error.message;
      passed = !testCase.expectSuccess && testCase.verify
        ? testCase.verify(output)
        : !testCase.expectSuccess;
    }

    if (passed) {
      console.log(colorize('green', '✅'));
    } else {
      console.log(colorize('red', '❌'));
      failed.push({ name: testCase.name, output: output.substring(0, 200) });
    }

    results.push({ name: testCase.name, passed });
  }

  const duration = ((Date.now() - startTime) / 1000).toFixed(1);
  const passedCount = results.filter(r => r.passed).length;
  const totalCount = results.length;

  if (failed.length > 0) {
    console.log(colorize('red', '\nFailed tests:'));
    for (const f of failed) {
      console.log(colorize('red', `  ❌   - debug ${f.name}`));
      console.log(`${colors.gray}    ${f.output}${colors.reset}`);
    }
  }

  if (passedCount === totalCount) {
    printSuccess(`Debug tests: ${passedCount}/${totalCount} passed (${duration}s)`);
    return { success: true, results, duration, passedCount, totalCount };
  } else {
    printError(`Debug tests: ${passedCount}/${totalCount} passed (${duration}s)`);
    return { success: false, results, duration, passedCount, totalCount };
  }
}

module.exports = { runDebugTests };
