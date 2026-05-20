/**
 * Unit command — Run AUnit tests + optional method-level statement coverage.
 *
 * Calls the abapgit-agent ABAP backend (ZCL_ABGAGT_COMMAND_UNIT) at
 * POST /sap/bc/z_abapgit_agent/unit. The backend uses CL_AUCV_TASK and
 * CL_SCV_COVERAGE_API directly, bypassing the ADT cov:query endpoint that
 * returns zero on some systems. See memory/project_coverage_disabled_gze.md.
 */

const pathModule = require('path');
const fs = require('fs');
const { AbapHttp } = require('../utils/abap-http');
const { getCoverageConfig } = require('../config');
const { formatHttpError } = require('../utils/format-error');

function escapeXml(str) {
  return String(str)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&apos;');
}

/**
 * Look up coverage data for a production method given a test method name.
 * Heuristic: strip a leading "TEST_" from the test method name and find a
 * coverage entry whose name contains that suffix.
 *
 *   TEST_PARSE_CLAS  →  PARSE_CLAS                  (direct match)
 *   TEST_PARSE_CLAS  →  ZIF_FOO~PARSE_CLAS          (interface-prefixed)
 *
 * Returns null when no plausible match exists.
 */
function findMethodCoverage(testMethodName, coverageMethods) {
  if (!coverageMethods || coverageMethods.length === 0) return null;
  const upper = (testMethodName || '').toUpperCase();
  if (!upper) return null;

  const candidates = [];
  candidates.push(upper);
  if (upper.startsWith('TEST_')) candidates.push(upper.substring(5));

  for (const c of candidates) {
    const exact = coverageMethods.find(m => (m.name || '').toUpperCase() === c);
    if (exact) return exact;
    const interfacePrefixed = coverageMethods.find(m => {
      const n = (m.name || '').toUpperCase();
      return n.endsWith('~' + c) || n.endsWith('=>' + c);
    });
    if (interfacePrefixed) return interfacePrefixed;
  }
  return null;
}

/**
 * Build JUnit XML from class results.
 *
 * Each class becomes a <testsuite>; per-class coverage totals are emitted as
 * <properties> on the suite. Each test method becomes a <testcase>; if the
 * test method name maps to a production method covered in `coverage.methods`,
 * the per-method statement totals are emitted as <properties> on the testcase.
 */
function buildUnitJUnit(results) {
  const suites = results.map(res => {
    const { className, methods, coverage, thresholdFailure } = res;
    const testCount = methods.length;
    const failedMethods = methods.filter(m => !m.passed);
    const syntheticFailures = thresholdFailure ? 1 : 0;
    const totalFailures = failedMethods.length + syntheticFailures;
    const suiteLabel = className;

    const lines = [];

    if (coverage) {
      lines.push('    <properties>');
      lines.push(`      <property name="coverage.rate"          value="${coverage.statementRate}"/>`);
      lines.push(`      <property name="coverage.lines.total"   value="${coverage.totalStatements}"/>`);
      lines.push(`      <property name="coverage.lines.covered" value="${coverage.executedStatements}"/>`);
      lines.push('    </properties>');
    }

    if (testCount === 0) {
      lines.push(`    <testcase name="(no tests)" classname="${escapeXml(suiteLabel)}"/>`);
    } else {
      for (const m of methods) {
        const timeAttr = m.executionTime != null ? ` time="${m.executionTime}"` : '';
        const testClass = m.testClassName ? `${suiteLabel}.${m.testClassName}` : suiteLabel;
        const methodCov = coverage ? findMethodCoverage(m.name, coverage.methods || []) : null;

        const methodPropsLines = methodCov
          ? [
              '      <properties>',
              `        <property name="coverage.method.lines.total"   value="${methodCov.totalStatements}"/>`,
              `        <property name="coverage.method.lines.covered" value="${methodCov.executedStatements}"/>`,
              `        <property name="coverage.method.rate"          value="${methodCov.rate ?? 0}"/>`,
              '      </properties>',
            ]
          : [];

        if (m.passed) {
          if (methodPropsLines.length > 0) {
            lines.push(
              `    <testcase name="${escapeXml(m.name)}" classname="${escapeXml(testClass)}"${timeAttr}>\n` +
              methodPropsLines.join('\n') + '\n' +
              `    </testcase>`
            );
          } else {
            lines.push(`    <testcase name="${escapeXml(m.name)}" classname="${escapeXml(testClass)}"${timeAttr}/>`);
          }
        } else {
          const msg = escapeXml(m.title || 'Test failed');
          const bodyParts = [m.title || 'Test failed'];
          if (m.details && m.details.length) bodyParts.push(...m.details);
          const body = escapeXml(bodyParts.join('\n'));
          const inner =
            methodPropsLines.join('\n') + (methodPropsLines.length > 0 ? '\n' : '') +
            `      <failure type="${escapeXml(m.kind || 'failedAssertion')}" message="${msg}">${body}</failure>`;
          lines.push(
            `    <testcase name="${escapeXml(m.name)}" classname="${escapeXml(testClass)}"${timeAttr}>\n` +
            inner + '\n' +
            `    </testcase>`
          );
        }
      }
    }

    if (thresholdFailure) {
      lines.push(
        `    <testcase name="coverage_threshold" classname="${escapeXml(suiteLabel)}">\n` +
        `      <failure type="FAILURE" message="${escapeXml(thresholdFailure)}">${escapeXml(thresholdFailure)}</failure>\n` +
        `    </testcase>`
      );
    }

    return (
      `  <testsuite name="${escapeXml(suiteLabel)}" ` +
      `tests="${Math.max(testCount + syntheticFailures, 1)}" failures="${totalFailures}" errors="0">\n` +
      lines.join('\n') + '\n' +
      `  </testsuite>`
    );
  });

  return (
    '<?xml version="1.0" encoding="UTF-8"?>\n' +
    '<testsuites>\n' +
    suites.join('\n') + '\n' +
    '</testsuites>\n'
  );
}

/**
 * Map a backend `ts_class_result` (snake_case) to the internal CLI shape
 * (camelCase) used by `buildUnitJUnit` and the console renderer.
 */
function normalizeBackendResult(raw, originalFile) {
  const className = raw.class_name || '';
  const methods = (raw.methods || []).map(m => ({
    testClassName: m.test_class_name,
    name: m.name,
    passed: !!m.passed,
    executionTime: typeof m.execution_time === 'number' ? m.execution_time : 0,
    kind: m.kind || undefined,
    title: m.title || undefined,
    details: Array.isArray(m.details) && m.details.length > 0 ? m.details : undefined,
    stack: Array.isArray(m.stack) && m.stack.length > 0
      ? m.stack.map(s => ({ name: s.name, line: s.line || null }))
      : undefined,
  }));

  const result = {
    className,
    file: raw.file || originalFile,
    methods,
  };

  if (raw.coverage_present && raw.coverage) {
    result.coverage = {
      totalStatements: raw.coverage.total_statements || 0,
      executedStatements: raw.coverage.executed_statements || 0,
      statementRate: typeof raw.coverage.statement_rate === 'number' ? raw.coverage.statement_rate : 0,
      methods: (raw.coverage.methods || []).map(mc => ({
        name: mc.name,
        totalStatements: mc.total_statements || 0,
        executedStatements: mc.executed_statements || 0,
        rate: typeof mc.rate === 'number' ? mc.rate : 0,
      })),
    };
  }

  return result;
}

/**
 * Print one class's results to the console.
 */
function printClassResult(result, coverage, verbose) {
  const { className, methods, coverage: cov } = result;
  const testCount = methods.length;
  const failedCount = methods.filter(m => !m.passed).length;
  const passedCount = testCount - failedCount;

  if (testCount === 0) {
    console.log(`  ➖ ${className} - No unit tests`);
  } else if (failedCount === 0) {
    console.log(`  ✅ ${className} - All tests passed`);
  } else {
    console.log(`  ❌ ${className} - Tests failed`);
  }
  console.log(`     Tests: ${testCount} | Passed: ${passedCount} | Failed: ${failedCount}`);

  if (coverage && cov) {
    const rate = cov.statementRate.toFixed ? cov.statementRate.toFixed(1) : cov.statementRate;
    console.log(`     📊 Coverage: ${rate}%   (${cov.executedStatements}/${cov.totalStatements} statements)`);
    if (verbose && cov.methods && cov.methods.length > 0) {
      console.log(`     Method coverage:`);
      const sorted = [...cov.methods].sort((a, b) => (a.name || '').localeCompare(b.name || ''));
      for (const mc of sorted) {
        const r = (mc.rate ?? 0).toFixed ? (mc.rate ?? 0).toFixed(1) : (mc.rate ?? 0);
        console.log(`       ${(mc.name || '').padEnd(48)} ${mc.executedStatements}/${mc.totalStatements} (${r}%)`);
      }
    }
  } else if (coverage && !cov) {
    console.log(`     📊 Coverage: unavailable`);
  }

  // Slow tests
  const slowThreshold = 0.1;
  const slowMethods = methods
    .filter(m => (m.executionTime || 0) > slowThreshold)
    .sort((a, b) => (b.executionTime || 0) - (a.executionTime || 0));
  if (slowMethods.length > 0) {
    const slowList = slowMethods.map(m => `${m.name} (${(m.executionTime || 0).toFixed(3)}s)`).join(', ');
    console.log(`     ⏱  Slowest: ${slowList}`);
  }

  if (failedCount > 0) {
    for (const m of methods.filter(mm => !mm.passed)) {
      const prefix = m.testClassName || className;
      console.log(`\n     ✗ ${prefix}=>${m.name}`);
      console.log(`       ${m.title || 'Test failed'}`);
      if (m.details && m.details.length) {
        for (const d of m.details) console.log(`       ${d}`);
      }
      if (m.stack && m.stack.length) {
        for (const s of m.stack) {
          const loc = s.line != null ? ` (line ${s.line})` : '';
          console.log(`         at ${s.name}${loc}`);
        }
      }
    }
    console.log('');
  }

  if (verbose && testCount > 0) {
    console.log(`     Method times:`);
    for (const m of methods) {
      const mark = m.passed ? '✓' : '✗';
      console.log(`       ${mark} ${m.name.padEnd(40)} ${(m.executionTime || 0).toFixed(3)}s`);
    }
    console.log('');
  }
}

module.exports = {
  name: 'unit',
  description: 'Run AUnit tests for ABAP test class files',
  requiresAbapConfig: true,

  async execute(args, context) {
    const { loadConfig } = context;

    if (args.includes('--help') || args.includes('-h')) {
      console.log(`
Usage:
  abapgit-agent unit --files <file1>,<file2>,... [options]

Description:
  Run AUnit tests for ABAP test class files (.testclasses.abap).
  Objects must be already active in the ABAP system (run pull first).
  Calls the abapgit-agent ABAP backend (ZCL_ABGAGT_COMMAND_UNIT), which uses
  CL_AUCV_TASK + CL_SCV_COVERAGE_API directly. Per-method coverage is included
  with --coverage.

Parameters:
  --files <file1,...>          Comma-separated .testclasses.abap files (required).
                               Multiple files referring to the same parent class
                               are de-duplicated automatically.
  --coverage                   Include per-method statement coverage.
  --coverage-threshold <N>     Fail/warn when coverage is below N percent (0–100).
                               Defaults to .abapgit-agent.json "coverage.threshold"
                               or 0 (off).
  --coverage-mode <warn|fail>  Action when below threshold. Defaults to project
                               config "coverage.mode" or "fail".
  --junit-output <file>        Write results as JUnit XML.
  --json                       Output as JSON.
  --verbose                    Show per-method execution times AND per-method
                               coverage; also print raw HTTP error responses.

Examples:
  abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap
  abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap --coverage --verbose
  abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap --coverage --coverage-threshold 80
  abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap --junit-output reports/unit.xml
`);
      return;
    }

    const jsonOutput = args.includes('--json');
    const verbose = args.includes('--verbose');

    const filesArgIndex = args.indexOf('--files');
    if (filesArgIndex === -1 || filesArgIndex + 1 >= args.length) {
      console.error('Error: --files parameter required');
      console.error('Usage: abapgit-agent unit --files <file1>,<file2>,... [--coverage] [--coverage-threshold <N>] [--junit-output <file>] [--json]');
      process.exit(1);
    }

    const files = args[filesArgIndex + 1].split(',').map(f => f.trim()).filter(Boolean);
    const coverage = args.includes('--coverage');

    // Coverage threshold/mode: CLI > project config > default
    const projectCoverage = getCoverageConfig();
    const coverageThresholdIdx = args.indexOf('--coverage-threshold');
    const coverageThreshold = coverageThresholdIdx !== -1
      ? parseInt(args[coverageThresholdIdx + 1], 10)
      : (projectCoverage.threshold || 0);
    const coverageModeIdx = args.indexOf('--coverage-mode');
    const coverageMode = coverageModeIdx !== -1
      ? args[coverageModeIdx + 1]
      : (projectCoverage.mode || 'fail');

    const junitArgIndex = args.indexOf('--junit-output');
    const junitOutput = junitArgIndex !== -1 ? args[junitArgIndex + 1] : null;

    if (!jsonOutput) {
      console.log(`\n  Running unit tests for ${files.length} file(s)${coverage ? ' (with coverage)' : ''}`);
      if (junitOutput) console.log(`  JUnit output: ${junitOutput}`);
      console.log('');
    }

    // Pre-check: every file must exist on disk (matches old behaviour)
    const missing = files.filter(f => !fs.existsSync(pathModule.isAbsolute(f) ? f : pathModule.join(process.cwd(), f)));
    if (missing.length > 0) {
      for (const f of missing) {
        if (!jsonOutput) console.error(`  ❌ File not found: ${f}`);
      }
      if (jsonOutput) {
        console.log(JSON.stringify(missing.map(f => ({ error: 'File not found', file: f })), null, 2));
      }
      process.exit(1);
    }

    const config = loadConfig();
    const http = new AbapHttp(config);
    await http.fetchCsrfToken();

    let backendResponse;
    try {
      backendResponse = await http.post(
        '/sap/bc/z_abapgit_agent/unit',
        { files, coverage },
        { csrfToken: http.csrfToken }
      );
    } catch (error) {
      if (!jsonOutput) {
        console.error(`\n  ❌ Error: ${formatHttpError(error)}`);
        if (verbose && error.body) {
          console.error('\n--- Raw response body ---');
          console.error(typeof error.body === 'object' ? JSON.stringify(error.body, null, 2) : String(error.body));
          console.error('--- End of response body ---');
        }
      } else {
        console.log(JSON.stringify({ error: error.message || 'Unknown error', statusCode: error.statusCode || 500 }, null, 2));
      }
      process.exit(1);
    }

    const rawResults = (backendResponse && backendResponse.results) || [];
    const results = rawResults.map(r => normalizeBackendResult(r, ''));

    let hasErrors = false;
    if (results.length === 0) {
      if (!jsonOutput) console.error('  ❌ No valid test classes found in --files input.');
      process.exit(1);
    }

    if (!jsonOutput) {
      for (const r of results) {
        printClassResult(r, coverage, verbose);
      }
    }

    for (const r of results) {
      if (r.methods && r.methods.some(m => !m.passed)) hasErrors = true;
    }

    // Coverage threshold enforcement — per class
    if (coverage && coverageThreshold > 0) {
      let anyData = false;
      for (const r of results) {
        if (!r.coverage) continue;
        anyData = true;
        const total = r.coverage.totalStatements;
        if (total === 0) continue;
        const rate = Math.round(r.coverage.executedStatements / total * 100);
        if (rate < coverageThreshold) {
          const msg = `${r.className}: coverage ${rate}% is below threshold ${coverageThreshold}%`;
          if (coverageMode === 'warn') {
            if (!jsonOutput) console.warn(`⚠️  ${msg}`);
          } else {
            if (!jsonOutput) console.error(`❌ ${msg}`);
            hasErrors = true;
            r.thresholdFailure = msg;
          }
        } else {
          if (!jsonOutput) console.log(`✅ ${r.className}: coverage ${rate}% meets threshold ${coverageThreshold}%`);
        }
      }
      if (!anyData && !jsonOutput) {
        console.warn('⚠️  Coverage data unavailable — threshold not enforced');
      }
    }

    // Failed tests summary
    if (!jsonOutput) {
      const allFailed = results.flatMap(r =>
        (r.methods || []).filter(m => !m.passed).map(m => ({ ...m, className: r.className }))
      );
      if (allFailed.length > 0) {
        console.log('\nFailed Tests:');
        console.log('─'.repeat(80));
        for (const m of allFailed) {
          const prefix = m.testClassName || m.className;
          console.log(`   ✗ ${prefix}=>${m.name}`);
          console.log(`     ${m.title || 'Test failed'}`);
          if (m.details && m.details.length) {
            for (const d of m.details) console.log(`     ${d}`);
          }
          console.log('');
        }
      }
    }

    if (junitOutput) {
      const xml = buildUnitJUnit(results);
      const outputPath = pathModule.isAbsolute(junitOutput)
        ? junitOutput
        : pathModule.join(process.cwd(), junitOutput);
      const dir = pathModule.dirname(outputPath);
      if (!fs.existsSync(dir)) fs.mkdirSync(dir, { recursive: true });
      fs.writeFileSync(outputPath, xml, 'utf8');
      if (!jsonOutput) console.log(`  JUnit report written to: ${outputPath}`);
    }

    if (jsonOutput) {
      console.log(JSON.stringify(results, null, 2));
    }

    if (hasErrors) {
      if (!jsonOutput) console.error('\n❌ Unit tests completed with errors');
      process.exit(1);
    }
  },

  // Exported for testing
  _buildUnitJUnit: buildUnitJUnit,
  _normalizeBackendResult: normalizeBackendResult,
  _findMethodCoverage: findMethodCoverage,
};
