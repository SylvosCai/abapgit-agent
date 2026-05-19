/**
 * Unit command - Run AUnit tests via ADT /sap/bc/adt/abapunit/testruns
 */

const pathModule = require('path');
const fs = require('fs');
const { AdtHttp } = require('../utils/adt-http');
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
 * Build the ADT AUnit run configuration XML for one class URI.
 */
function buildRunConfigXml(adtUri, coverage) {
  return `<?xml version="1.0" encoding="UTF-8"?>
<aunit:runConfiguration xmlns:aunit="http://www.sap.com/adt/aunit">
  <external>
    <coverage active="${coverage ? 'true' : 'false'}"/>
  </external>
  <options>
    <uriType value="semantic"/>
    <testDeterminationStrategy sameProgram="true" assignedTests="false"/>
    <testRiskLevels harmless="true" dangerous="false" critical="false"/>
    <testDurations short="true" medium="false" long="false"/>
    <withNavigationUri enabled="true"/>
  </options>
  <adtcore:objectSets xmlns:adtcore="http://www.sap.com/adt/core">
    <objectSet kind="inclusive">
      <adtcore:objectReferences>
        <adtcore:objectReference adtcore:uri="${adtUri}"/>
      </adtcore:objectReferences>
    </objectSet>
  </adtcore:objectSets>
</aunit:runConfiguration>`;
}

/**
 * Parse the ADT AUnit run result XML into a structured result object.
 *
 * Returns:
 *   {
 *     className,
 *     methods: [{
 *       name, testClassName, passed, executionTime,
 *       kind?, title?, details?: string[], stack?: [{name, uri}]
 *     }],
 *     coverageStats: { totalLines, coveredLines, coverageRate } | null
 *   }
 */
function extractAlert(block) {
  const kindMatch = block.match(/<alert\s[^>]*kind="([^"]*)"/);
  const kind = kindMatch ? kindMatch[1] : 'failedAssertion';
  const titleMatch = block.match(/<title[^>]*>([^<]*)<\/title>/);
  const title = titleMatch ? titleMatch[1].trim() : 'Test failed';
  const details = [];
  const detailRe = /<detail\s[^>]*text="([^"]*)"/g;
  let dm;
  while ((dm = detailRe.exec(block)) !== null) {
    const t = dm[1].trim();
    if (t) details.push(t);
  }
  const stack = [];
  const stackRe = /<stackEntry\s([^>]*\/?>)/g;
  let sm;
  while ((sm = stackRe.exec(block)) !== null) {
    const entry = sm[1];
    const snMatch = entry.match(/adtcore:name="([^"]*)"/);
    const suMatch = entry.match(/adtcore:uri="([^"]*)"/);
    if (snMatch) {
      const lineMatch = suMatch && suMatch[1].match(/#start=(\d+)/);
      stack.push({ name: snMatch[1], line: lineMatch ? parseInt(lineMatch[1], 10) : null });
    }
  }
  return { kind, title, details, stack };
}

function parseRunResult(xml, className) {
  const methods = [];

  // Split on <testClass blocks to capture test class name per method
  const testClassBlocks = xml.split(/<testClass\s/);
  for (let ci = 1; ci < testClassBlocks.length; ci++) {
    const classBlock = testClassBlocks[ci];

    const classNameMatch = classBlock.match(/adtcore:name="([^"]*)"/);
    const testClassName = classNameMatch ? classNameMatch[1] : className;

    // Check for class-level alerts (e.g. SETUP/TEARDOWN failures) that appear before <testMethods>
    const beforeMethods = classBlock.split(/<testMethods[\s>]/)[0];
    const classAlertsMatch = beforeMethods.match(/<alerts>([\s\S]*?)<\/alerts>/);
    if (classAlertsMatch && classAlertsMatch[1].trim()) {
      const alert = extractAlert(beforeMethods);
      // Determine synthetic method name from stack (look for SETUP/TEARDOWN in description)
      let syntheticName = 'SETUP';
      for (const s of alert.stack) {
        const descMatch = s.name && s.name.match(/\(([^)]+)\)\s*$/);
        if (descMatch) { syntheticName = descMatch[1].trim(); break; }
      }
      methods.push({ name: syntheticName, testClassName, passed: false, executionTime: 0, ...alert });
    }

    // Split methods within this test class block
    const methodBlocks = classBlock.split(/<testMethod\s/);
    for (let i = 1; i < methodBlocks.length; i++) {
      const block = methodBlocks[i];

      const nameMatch = block.match(/adtcore:name="([^"]*)"/);
      if (!nameMatch) continue;
      const name = nameMatch[1];

      const execTimeMatch = block.match(/executionTime="([^"]*)"/);
      const executionTime = execTimeMatch ? parseFloat(execTimeMatch[1]) : 0;

      // Find the <alerts> section
      const alertsMatch = block.match(/<alerts>([\s\S]*?)<\/alerts>/);
      const alertsContent = alertsMatch ? alertsMatch[1].trim() : '';

      if (!alertsContent || alertsContent === '<alerts/>') {
        methods.push({ name, testClassName, passed: true, executionTime });
        continue;
      }

      const { kind, title, details, stack } = extractAlert(block);
      methods.push({ name, testClassName, passed: false, executionTime, kind, title, details, stack });
    }
  }

  // Extract coverage measurement URI from <external><coverage adtcore:uri="..."/></external>
  let coverageMeasurementUri = null;
  const covUriMatch = xml.match(/coverage[^>]*adtcore:uri="([^"]*)"/);
  if (covUriMatch) coverageMeasurementUri = covUriMatch[1];

  return { className, methods, coverageStats: null, coverageMeasurementUri };
}

/**
 * Fetch coverage statistics for a class by querying the ADT coverage measurement URI.
 *
 * The AUnit response contains a measurement URI in <external><coverage adtcore:uri="..."/></external>.
 * Posting a cov:query to that URI returns statement/branch/procedure coverage totals.
 *
 * Returns { totalStatements, executedStatements, statementRate } or null if unavailable.
 */
async function fetchCoverageStats(adt, measurementUri, classAdtUri, className) {
  try {
    const resp = await adt.post(
      measurementUri,
      `<cov:query xmlns:cov="http://www.sap.com/adt/cov" xmlns:adtcore="http://www.sap.com/adt/core">` +
      `<adtcore:objectSets>` +
      `<adtcore:objectSet kind="inclusive">` +
      `<adtcore:objectReferences>` +
      `<adtcore:objectReference adtcore:uri="${classAdtUri}" adtcore:name="${className}" adtcore:type="CLAS/OC"/>` +
      `</adtcore:objectReferences>` +
      `</adtcore:objectSet>` +
      `</adtcore:objectSets>` +
      `</cov:query>`,
      { contentType: 'application/xml', accept: 'application/xml' }
    );
    const xml = resp.body || '';
    // Parse <coverage type="statement" total="N" executed="N"/>
    const stmtMatch = xml.match(/<coverage type="statement"[^>]*total="(\d+)"[^>]*executed="(\d+)"/);
    if (!stmtMatch) return null;
    const total = parseInt(stmtMatch[1], 10);
    const executed = parseInt(stmtMatch[2], 10);
    if (total === 0) return null;
    const rate = Math.round(executed / total * 100 * 10) / 10;
    return { totalLines: total, coveredLines: executed, coverageRate: rate };
  } catch (_) {
    return null;
  }
}

/**
 * Build JUnit XML from parsed ADT AUnit results.
 *
 * Each test method gets its own <testcase> element (pass or fail).
 * Coverage stats (if present) are emitted as <properties> on the testsuite.
 * A synthetic coverage_threshold <failure> is injected when threshold is breached.
 */
function buildUnitJUnit(results) {
  const suites = results.map(res => {
    const { className, methods, coverageStats, thresholdFailure } = res;
    const testCount = methods.length;
    const failedMethods = methods.filter(m => !m.passed);
    const syntheticFailures = thresholdFailure ? 1 : 0;
    const totalFailures = failedMethods.length + syntheticFailures;
    // classname "ZCL_MY_TEST.LTCL_UNIT_TEST" groups under the same ABAP class node
    // as "ZCL_MY_TEST.Inspect" from the inspect report — one node per ABAP class.
    const suiteLabel = className;

    const lines = [];

    if (coverageStats) {
      lines.push('    <properties>');
      lines.push(`      <property name="coverage.rate"          value="${coverageStats.coverageRate}"/>`);
      lines.push(`      <property name="coverage.lines.total"   value="${coverageStats.totalLines}"/>`);
      lines.push(`      <property name="coverage.lines.covered" value="${coverageStats.coveredLines}"/>`);
      lines.push('    </properties>');
    }

    if (testCount === 0) {
      lines.push(`    <testcase name="(no tests)" classname="${escapeXml(suiteLabel)}"/>`);
    } else {
      for (const m of methods) {
        const timeAttr = m.executionTime != null ? ` time="${m.executionTime}"` : '';
        // "Unit Tests / ZCL_MY_TEST.LTCL_UNIT_TEST" groups by namespace → class → local test class.
        const testClass = m.testClassName ? `${suiteLabel}.${m.testClassName}` : suiteLabel;
        if (m.passed) {
          lines.push(`    <testcase name="${escapeXml(m.name)}" classname="${escapeXml(testClass)}"${timeAttr}/>`);
        } else {
          const msg = escapeXml(m.title || 'Test failed');
          const bodyParts = [m.title || 'Test failed'];
          if (m.details && m.details.length) bodyParts.push(...m.details);
          const body = escapeXml(bodyParts.join('\n'));
          lines.push(
            `    <testcase name="${escapeXml(m.name)}" classname="${escapeXml(testClass)}"${timeAttr}>\n` +
            `      <failure type="${escapeXml(m.kind || 'failedAssertion')}" message="${msg}">${body}</failure>\n` +
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
 * Run AUnit tests for a single .testclasses.abap file via ADT.
 */
async function runUnitTestForFile(sourceFile, adt, coverage, jsonOutput, verbose) {
  if (!jsonOutput) {
    console.log(`  Running unit test for: ${sourceFile}`);
  }

  const absolutePath = pathModule.isAbsolute(sourceFile)
    ? sourceFile
    : pathModule.join(process.cwd(), sourceFile);

  if (!fs.existsSync(absolutePath)) {
    if (!jsonOutput) console.error(`  ❌ File not found: ${absolutePath}`);
    return { error: 'File not found', statusCode: 404, file: sourceFile };
  }

  const fileName = pathModule.basename(sourceFile);
  if (!fileName.toLowerCase().includes('.testclasses.abap')) {
    if (!jsonOutput) console.error(`  ❌ Invalid file format: ${sourceFile}`);
    return { error: 'Invalid file format: must be .testclasses.abap', statusCode: 400, file: sourceFile };
  }

  const className = fileName.split('.')[0].toUpperCase();
  const adtUri = `/sap/bc/adt/oo/classes/${className.toLowerCase()}`;
  const xmlBody = buildRunConfigXml(adtUri, coverage);

  try {
    const resp = await adt.post('/sap/bc/adt/abapunit/testruns', xmlBody, {
      contentType: 'application/*',
      accept: 'application/*',
    });

    const result = parseRunResult(resp.body || '', className);

    // Fetch coverage stats via the measurement URI (second API call)
    if (coverage && result.coverageMeasurementUri) {
      result.coverageStats = await fetchCoverageStats(adt, result.coverageMeasurementUri, adtUri, className);
    }

    const testCount = result.methods.length;
    const failedCount = result.methods.filter(m => !m.passed).length;
    const passedCount = testCount - failedCount;

    if (!jsonOutput) {
      if (testCount === 0) {
        console.log(`  ➖ ${className} - No unit tests`);
      } else if (failedCount === 0) {
        console.log(`  ✅ ${className} - All tests passed`);
      } else {
        console.log(`  ❌ ${className} - Tests failed`);
      }
      console.log(`     Tests: ${testCount} | Passed: ${passedCount} | Failed: ${failedCount}`);

      if (coverage && result.coverageStats) {
        const { totalLines, coveredLines, coverageRate } = result.coverageStats;
        console.log(`     📊 Coverage: ${coverageRate}%`);
        console.log(`        Total Lines: ${totalLines}`);
        console.log(`        Covered Lines: ${coveredLines}`);
      }

      // Slow test summary: show methods with executionTime > 0.1s
      const slowThreshold = 0.1;
      const slowMethods = result.methods
        .filter(m => m.executionTime > slowThreshold)
        .sort((a, b) => b.executionTime - a.executionTime);
      if (slowMethods.length > 0) {
        const slowList = slowMethods.map(m => `${m.name} (${m.executionTime.toFixed(3)}s)`).join(', ');
        console.log(`     ⏱  Slowest: ${slowList}`);
      }

      if (failedCount > 0) {
        for (const m of result.methods.filter(m => !m.passed)) {
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

      // --verbose: per-method time table
      if (verbose && testCount > 0) {
        console.log(`     Method times:`);
        for (const m of result.methods) {
          const mark = m.passed ? '✓' : '✗';
          console.log(`       ${mark} ${m.name.padEnd(40)} ${(m.executionTime || 0).toFixed(3)}s`);
        }
        console.log('');
      }
    }

    return result;
  } catch (error) {
    const errorResponse = {
      error: error.message || 'Unknown error',
      statusCode: error.statusCode || 500,
      file: sourceFile,
      className,
      methods: [],
      coverageStats: null,
    };
    if (!jsonOutput) {
      console.error(`\n  ❌ Error: ${formatHttpError(error)}`);
      if (verbose && error.body) {
        console.error('\n--- Raw response body ---');
        console.error(typeof error.body === 'object' ? JSON.stringify(error.body, null, 2) : String(error.body));
        console.error('--- End of response body ---');
      }
    }
    return errorResponse;
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
  Calls the standard ADT endpoint /sap/bc/adt/abapunit/testruns directly.

Parameters:
  --files <file1,...>          Comma-separated .testclasses.abap files (required).
  --coverage                   Include code coverage data in output.
  --coverage-threshold <N>     Fail/warn when coverage is below N percent (0–100). Default: 0 (off).
  --coverage-mode <warn|fail>  Action when below threshold: warn = UNSTABLE, fail = error. Default: fail.
  --junit-output <file>        Write results as JUnit XML to this file.
  --json                       Output as JSON.
  --verbose                    Show per-method execution times; also print raw HTTP error responses.

Examples:
  abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap
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

    const files = args[filesArgIndex + 1].split(',').map(f => f.trim());
    const coverage = args.includes('--coverage');

    const coverageThresholdIdx = args.indexOf('--coverage-threshold');
    const coverageThreshold = coverageThresholdIdx !== -1 ? parseInt(args[coverageThresholdIdx + 1], 10) : 0;

    const coverageModeIdx = args.indexOf('--coverage-mode');
    const coverageMode = coverageModeIdx !== -1 ? args[coverageModeIdx + 1] : 'fail';

    const junitArgIndex = args.indexOf('--junit-output');
    const junitOutput = junitArgIndex !== -1 ? args[junitArgIndex + 1] : null;

    if (!jsonOutput) {
      console.log(`\n  Running unit tests for ${files.length} file(s)${coverage ? ' (with coverage)' : ''}`);
      if (junitOutput) console.log(`  JUnit output: ${junitOutput}`);
      console.log('');
    }

    const config = loadConfig();
    const adt = new AdtHttp(config);
    await adt.fetchCsrfToken();

    const results = [];
    let hasErrors = false;

    for (const sourceFile of files) {
      const result = await runUnitTestForFile(sourceFile, adt, coverage, jsonOutput, verbose);
      results.push(result);

      if (result.error || result.statusCode >= 400) {
        hasErrors = true;
      }
      const methods = result.methods || [];
      if (methods.some(m => !m.passed)) hasErrors = true;
    }

    // Coverage threshold enforcement — per file
    if (coverage && coverageThreshold > 0) {
      let anyData = false;
      for (const result of results) {
        if (!result.coverageStats) continue;
        anyData = true;
        const { totalLines, coveredLines, coverageRate } = result.coverageStats;
        if (totalLines === 0) continue;
        const className = result.className || 'UNKNOWN';
        const rate = Math.round(coveredLines / totalLines * 100);
        if (rate < coverageThreshold) {
          const msg = `${className}: coverage ${rate}% is below threshold ${coverageThreshold}%`;
          if (coverageMode === 'warn') {
            if (!jsonOutput) console.warn(`⚠️  ${msg}`);
          } else {
            if (!jsonOutput) console.error(`❌ ${msg}`);
            hasErrors = true;
            result.thresholdFailure = msg;
          }
        } else {
          if (!jsonOutput) console.log(`✅ ${className}: coverage ${rate}% meets threshold ${coverageThreshold}%`);
        }
      }
      if (!anyData && !jsonOutput) {
        console.warn('⚠️  Coverage data unavailable — threshold not enforced');
      }
    }

    // Failed tests summary
    if (!jsonOutput) {
      const allFailed = results.flatMap(r => (r.methods || []).filter(m => !m.passed).map(m => ({ ...m, className: r.className })));
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

    // JUnit output
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

    // JSON output
    if (jsonOutput) {
      console.log(JSON.stringify(results, null, 2));
    }

    if (hasErrors) {
      if (!jsonOutput) console.error('\n❌ Unit tests completed with errors');
      process.exit(1);
    }
  },

  // Exported for testing
  _buildRunConfigXml: buildRunConfigXml,
  _parseRunResult: parseRunResult,
  _buildUnitJUnit: buildUnitJUnit,
  _fetchCoverageStats: fetchCoverageStats,
};
