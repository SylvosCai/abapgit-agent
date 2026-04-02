/**
 * Debug Command Test Runner
 *
 * Tests ABAP debugger — breakpoint management across all object types.
 * Requires the abgagt-debug-test repo checked out at ../abgagt-debug-test
 * (sibling of the abapgit-agent repo).
 *
 * Objects used:
 *   ZCL_CAIS_DBG_TRIGGER — trigger class (CLAS main, testclasses, locals_imp)
 *   LZCAIS_DBG_TESTU01   — FUGR include of ZCAIS_DBG_TEST
 *
 * Group A (13 tests): Hardcoded known-good lines — verify ADT accepts them and
 *   returns the expected LINE_NR (include-relative) and line (global) values.
 *
 * Group B (16 tests): view --full --lines → parse hint → debug set → debug list
 *   round-trip for CLAS main, testclasses, locals_imp, and FUGR include.
 *   Catches regressions where view --lines emits wrong line numbers.
 *
 * Group C (6 tests): Unexecutable line rejection — comments, blank lines, DATA
 *   declarations, METHOD statements. ADT must exit 1 with stale JSON.
 *
 * Confirmed ADT-accepted lines (probed via direct POST, 2026-04-02):
 *   ZCL_CAIS_DBG_TRIGGER main:        line 33, LINE_NR=5  (lv_sum = iv_a + iv_b)
 *   ZCL_CAIS_DBG_TRIGGER testclasses: line 13, LINE_NR=13 (rv_val = iv_val * lv_two)
 *   ZCL_CAIS_DBG_TRIGGER locals_imp:  line 6,  LINE_NR=6  (lv_label = 'debug-test')
 *   LZCAIS_DBG_TESTU01 (FUGR):        line 13, LINE_NR=13 (lv_result = iv_a + iv_b)
 */

'use strict';

const { execSync } = require('child_process');
const path = require('path');
const fs = require('fs');

const TEST_REPO_URL = 'https://github.tools.sap/I045696/abgagt-debug-test.git';

// Helper: parse LINE_NR from ADT id returned in --json output.
// NOTE: LINE_NR is include-relative, not the global assembled-source line.
// For CLAS main methods it differs from the line we passed in (e.g. global 33 → LINE_NR 5).
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
//   * ---- Method: COMPUTE (CM001) — breakpoint: debug set --objects ZCL_CAIS_DBG_TRIGGER:33 ----
//   * ---- FM: ZCAIS_DBG_ADD (LZCAIS_DBG_TESTU01) — breakpoint: debug set --objects LZCAIS_DBG_TESTU01:13 ----
//
// For CLAS main methods the source lines use G [N] format:
//     33 [  5]      lv_sum = iv_a + iv_b.
// where G=33 is the global assembled-source line (sent to ADT as #start=)
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

function buildTestCases(debugRepoPath) {
  // Shared state for Group B — view output is parsed in verify() and consumed
  // by the next test's args() / verify(). Tests run sequentially so this is safe.
  const viewParsed = {
    clasMain: null,
    testclasses: null,
    localsImp: null,
    fugr: null,
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
      name: '[A] debug set CLAS main — line 33 accepted by ADT (LINE_NR=5)',
      // Global line 33 = CM001 include line 5 (as shown by view --lines: "33 [  5]")
      args: ['set', '--object', 'ZCL_CAIS_DBG_TRIGGER', '--line', '33', '--json'],
      expectSuccess: true,
      verify: (output) => adtLineNr(output) === 5 && jsonLine(output) === 33
    },
    {
      name: '[A] debug list shows CLAS main breakpoint',
      args: ['list'],
      expectSuccess: true,
      verify: (output) => output.includes('ZCL_CAIS_DBG_TRIGGER') && output.includes('33')
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
      verify: (output) => !output.includes('ZCL_CAIS_DBG_TRIGGER')
    },

    // --- testclasses include breakpoint ---
    {
      name: '[A] debug set testclasses — line 13 accepted by ADT (LINE_NR=13)',
      // (skips comment on line 10, DATA on line 11, blank on line 12)
      args: ['set', '--objects', 'ZCL_CAIS_DBG_TRIGGER:13', '--include', 'testclasses', '--json'],
      expectSuccess: true,
      verify: (output) => adtLineNr(output) === 13 && jsonLine(output) === 13
    },
    {
      name: '[A] debug list shows testclasses breakpoint',
      args: ['list'],
      expectSuccess: true,
      verify: (output) => output.includes('ZCL_CAIS_DBG_TRIGGER') && output.includes('13')
    },

    // --- locals_imp include breakpoint ---
    {
      name: '[A] debug set locals_imp — line 6 accepted by ADT (LINE_NR=6)',
      // (skips comment on line 3, DATA on line 4, blank on line 5)
      args: ['set', '--objects', 'ZCL_CAIS_DBG_TRIGGER:6', '--include', 'locals_imp', '--json'],
      expectSuccess: true,
      verify: (output) => adtLineNr(output) === 6 && jsonLine(output) === 6
    },
    {
      name: '[A] debug list shows both testclasses and locals_imp breakpoints',
      args: ['list'],
      expectSuccess: true,
      verify: (output) =>
        output.includes('ZCL_CAIS_DBG_TRIGGER') &&
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
    // LZCAIS_DBG_TESTU01 is the FM source include of ZCAIS_DBG_TEST.
    // Before the fix, this was routed to /programs/includes/ (wrong path).
    // Correct path: /functions/groups/zcais_dbg_test/includes/lzcais_dbg_testu01/source/main
    {
      name: '[A] debug set FUGR include — line 13 accepted by ADT (LINE_NR=13)',
      args: ['set', '--objects', 'LZCAIS_DBG_TESTU01:13', '--json'],
      expectSuccess: true,
      verify: (output) => adtLineNr(output) === 13 && jsonLine(output) === 13
    },
    {
      name: '[A] debug list shows FUGR include breakpoint',
      args: ['list'],
      expectSuccess: true,
      verify: (output) => output.includes('LZCAIS_DBG_TESTU01') && output.includes('13')
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
      args: ['--objects', 'ZCL_CAIS_DBG_TRIGGER', '--full', '--lines'],
      expectSuccess: true,
      verify: (output) => {
        const hint = parseViewHint(output, 'COMPUTE');
        if (!hint) return false;
        viewParsed.clasMain = hint;
        return hint.object === 'ZCL_CAIS_DBG_TRIGGER' &&
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
        return output.includes('ZCL_CAIS_DBG_TRIGGER') && h !== null && output.includes(String(h.line));
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
      args: ['--objects', 'ZCL_CAIS_DBG_TRIGGER', '--full', '--lines'],
      expectSuccess: true,
      verify: (output) => {
        const hint = parseViewHint(output, 'DOUBLE');
        if (!hint) return false;
        viewParsed.testclasses = hint;
        return hint.object === 'ZCL_CAIS_DBG_TRIGGER' &&
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
        return output.includes('ZCL_CAIS_DBG_TRIGGER') && h !== null && output.includes(String(h.line));
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
      args: ['--objects', 'ZCL_CAIS_DBG_TRIGGER', '--full', '--lines'],
      expectSuccess: true,
      verify: (output) => {
        const hint = parseViewHint(output, 'DESCRIBE');
        if (!hint) return false;
        viewParsed.localsImp = hint;
        return hint.object === 'ZCL_CAIS_DBG_TRIGGER' &&
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
        return output.includes('ZCL_CAIS_DBG_TRIGGER') && h !== null && output.includes(String(h.line));
      }
    },
    {
      name: '[B] debug delete --all (cleanup after B3)',
      args: ['delete', '--all'],
      expectSuccess: true,
      verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
    },

    // B4: FUGR FM (ZCAIS_DBG_ADD)
    {
      name: '[B] view --full --lines FUGR: parse ZCAIS_DBG_ADD FM hint',
      run: 'view',
      args: ['--objects', 'ZCAIS_DBG_TEST', '--type', 'FUGR', '--full', '--fm', 'ZCAIS_DBG_ADD', '--lines'],
      expectSuccess: true,
      verify: (output) => {
        const hint = parseViewHint(output, 'ZCAIS_DBG_ADD');
        if (!hint) return false;
        viewParsed.fugr = hint;
        return hint.object === 'LZCAIS_DBG_TESTU01' &&
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
        return output.includes('LZCAIS_DBG_TESTU01') && h !== null && output.includes(String(h.line));
      }
    },
    {
      name: '[B] debug delete --all (final cleanup)',
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
      args: ['set', '--objects', 'ZCL_CAIS_DBG_TRIGGER:16', '--json'],
      expectSuccess: false,
      verify: (output) =>
        output.includes('"error"') && output.includes('"stale"') &&
        output.includes('"line":16') && !output.includes('"id"')
    },
    {
      name: '[C] debug set CLAS main line 29 (METHOD stmt) — rejected by ADT',
      args: ['set', '--objects', 'ZCL_CAIS_DBG_TRIGGER:29', '--json'],
      expectSuccess: false,
      verify: (output) =>
        output.includes('"error"') && output.includes('"stale"') && output.includes('"line":29')
    },
    {
      name: '[C] debug set testclasses line 9 (METHOD stmt) — rejected by ADT',
      args: ['set', '--objects', 'ZCL_CAIS_DBG_TRIGGER:9', '--include', 'testclasses', '--json'],
      expectSuccess: false,
      verify: (output) =>
        output.includes('"error"') && output.includes('"stale"') && output.includes('"line":9')
    },
    {
      name: '[C] debug set locals_imp line 2 (METHOD stmt) — rejected by ADT',
      args: ['set', '--objects', 'ZCL_CAIS_DBG_TRIGGER:2', '--include', 'locals_imp', '--json'],
      expectSuccess: false,
      verify: (output) =>
        output.includes('"error"') && output.includes('"stale"') && output.includes('"line":2')
    },
    {
      name: '[C] debug set FUGR line 2 (comment) — rejected by ADT',
      args: ['set', '--objects', 'LZCAIS_DBG_TESTU01:2', '--json'],
      expectSuccess: false,
      verify: (output) =>
        output.includes('"error"') && output.includes('"stale"') && output.includes('"line":2')
    },
    {
      name: '[C] debug set FUGR line 12 (blank line) — rejected by ADT',
      args: ['set', '--objects', 'LZCAIS_DBG_TESTU01:12', '--json'],
      expectSuccess: false,
      verify: (output) =>
        output.includes('"error"') && output.includes('"stale"') && output.includes('"line":12')
    },
  ];
}

/**
 * Run debug integration tests.
 * @param {string} repoRoot    - abapgit-agent repo root (for bin/abapgit-agent)
 * @param {object} helpers     - { printSubHeader, printSuccess, printError, printWarning, printInfo, colorize, colors }
 */
function runDebugTests(repoRoot, helpers) {
  const { printSubHeader, printSuccess, printError, printWarning, printInfo, colorize, colors } = helpers;

  printSubHeader('Running Debug Tests (breakpoint management)');

  // Clone into output/abgagt-debug-test (same pattern as lifecycle-runner)
  const outputDir = path.join(repoRoot, 'output');
  if (!fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir, { recursive: true });
  }

  const debugRepoPath = path.join(outputDir, 'abgagt-debug-test');
  if (fs.existsSync(debugRepoPath)) {
    printInfo('Debug test repo already exists, using existing copy');
  } else {
    printInfo(`Cloning debug test repo: ${TEST_REPO_URL}`);
    try {
      execSync(`git clone ${TEST_REPO_URL} ${debugRepoPath}`, { encoding: 'utf8' });
    } catch (e) {
      printError(`Failed to clone debug test repo: ${e.message}`);
      return { success: false, skipped: false, results: [], duration: '0.0', passedCount: 0, totalCount: 0 };
    }
  }

  const agentBin = path.join(repoRoot, 'bin', 'abapgit-agent');
  const testCases = buildTestCases(debugRepoPath);
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
        { cwd: debugRepoPath, encoding: 'utf8', timeout: 120000 }
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
