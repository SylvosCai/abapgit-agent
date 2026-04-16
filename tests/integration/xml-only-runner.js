/**
 * XML-Only Object Pull Integration Test Runner
 * Tests that TABL, DTEL, and TTYP objects can be selectively pulled via --files
 * using their .xml file paths (no .abap source file exists for these types).
 *
 * Test Repository: abgagt-pull-test (URL configured via testRepos.pull in .abapGitAgent)
 *   Branch: feature/xml-only-test
 *     - src/zabgagt_xo_dtel.dtel.xml  (DTEL — 15 chars, within DDIC limit)
 *     - src/zabgagt_xo_tabl.tabl.xml  (TABL — 15 chars, within DDIC limit)
 *     - src/zabgagt_xo_ttyp.ttyp.xml  (TTYP — 15 chars, within DDIC limit)
 *
 * Test sequence:
 *
 *   Reset:   pull all 3 objects with --conflict-mode ignore  → idempotent baseline
 *
 *   Step 1:  pull --files src/zabgagt_xmlonly_tabl.tabl.xml
 *            verify: TABL ZABGAGT_XMLONLY_TABL activated, DTEL + TTYP absent
 *
 *   Step 2:  pull --files src/zabgagt_xmlonly_dtel.dtel.xml
 *            verify: DTEL ZABGAGT_XMLONLY_DTEL activated, TABL + TTYP absent
 *
 *   Step 3:  pull --files src/zabgagt_xmlonly_ttyp.ttyp.xml
 *            verify: TTYP ZABGAGT_XMLONLY_TTYP activated, TABL + DTEL absent
 *
 *   Step 4:  pull --files src/zabgagt_xmlonly_tabl.tabl.xml,src/zabgagt_xmlonly_dtel.dtel.xml
 *            verify: both TABL + DTEL activated, TTYP absent
 *
 *   Step 5:  pull --files src/zabgagt_xmlonly_tabl.tabl.xml (repeated) → still succeeds
 *            verify: idempotency — re-pulling an already-active object does not fail
 *
 *   Step 6:  CLI rejection — pass a 2-part XML file (.abapgit.xml)
 *            verify: process exits non-zero, error mentions "not recognised"
 *            (no ABAP call made — pure client-side validation)
 */

const { execSync } = require('child_process');
const path = require('path');

const { getTestRepoUrl } = require('./test-repos');
const TEST_REPO_URL = getTestRepoUrl('pull');
const TEST_BRANCH   = 'feature/xml-only-test';

const FILE_TABL = 'src/zabgagt_xo_tabl.tabl.xml';
const FILE_DTEL = 'src/zabgagt_xo_dtel.dtel.xml';
const FILE_TTYP = 'src/zabgagt_xo_ttyp.ttyp.xml';

const OBJ_TABL = 'ZABGAGT_XO_TABL';
const OBJ_DTEL = 'ZABGAGT_XO_DTEL';
const OBJ_TTYP = 'ZABGAGT_XO_TTYP';

/**
 * Run a pull command against the test repo using --url.
 * Works from any cwd — credentials come from .abapGitAgent in repoRoot.
 */
function runPull(repoRoot, files, extraArgs = []) {
  const filesArgs = files ? ['--files', files] : [];
  const cmd = [
    'node', path.join(repoRoot, 'bin', 'abapgit-agent'),
    'pull',
    '--url',    TEST_REPO_URL,
    '--branch', TEST_BRANCH,
    ...filesArgs,
    ...extraArgs
  ].join(' ');

  try {
    const output = execSync(cmd, { cwd: repoRoot, encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'], timeout: 120000 });
    return { output, exitCode: 0 };
  } catch (err) {
    return { output: (err.stdout || '') + (err.stderr || ''), exitCode: err.status || 1 };
  }
}

/**
 * Run xml-only integration tests.
 * @param {string} repoRoot - abapgit-agent repository root
 * @param {Object} printFunctions
 */
function runXmlOnlyTests(repoRoot, { printSubHeader, printInfo, printSuccess, printError, colorize }) {
  printSubHeader('Running XML-Only Object Pull Tests (TABL, DTEL, TTYP)');

  const startTime = Date.now();
  const results   = [];

  printInfo(`Test repo:   ${TEST_REPO_URL}`);
  printInfo(`Branch:      ${TEST_BRANCH}`);
  printInfo(`Objects:     ${OBJ_TABL} (TABL), ${OBJ_DTEL} (DTEL), ${OBJ_TTYP} (TTYP)`);
  printInfo('');

  const addResult = (name, passed, hint = '') => {
    results.push({ name, passed });
    if (passed) {
      printSuccess(`✓ ${name}`);
    } else {
      printError(`✗ ${name}${hint ? `\n  ${hint}` : ''}`);
    }
  };

  // ─── Reset: pull all 3 objects → idempotent baseline ─────────────────────────
  printInfo(colorize('cyan', 'Reset: pull all 3 objects with --conflict-mode ignore (idempotent baseline)'));
  {
    const { output, exitCode } = runPull(repoRoot, [FILE_TABL, FILE_DTEL, FILE_TTYP].join(','), ['--conflict-mode', 'ignore']);
    if (exitCode === 0 && output.includes('Pull completed')) {
      printSuccess('✓ baseline reset — all 3 objects active');
    } else {
      printError('✗ baseline reset failed — cannot continue');
      printError(`  ${output.split('\n').find(l => l.includes('Error') || l.includes('failed')) || output.split('\n')[0]}`);
      return { success: false, results, duration: '0.0', passedCount: 0, totalCount: 8 };
    }
  }
  printInfo('');

  // ─── Step 1: pull only TABL ───────────────────────────────────────────────────
  printInfo(colorize('cyan', `Step 1: pull --files ${FILE_TABL}  (TABL only)`));
  {
    const { output, exitCode } = runPull(repoRoot, FILE_TABL);

    addResult('[TABL] pull succeeds',
      exitCode === 0 && output.includes('Pull completed'),
      `exitCode=${exitCode}\n  ${output.split('\n').find(l => l.includes('Error')) || ''}`);

    addResult('[TABL] ZABGAGT_XO_TABL processed by abapGit',
      output.includes(`TABL ${OBJ_TABL}`) || output.includes('object already active'),
      `"TABL ${OBJ_TABL}" not in pull log and no "already active" message`);

    addResult('[TABL] DTEL and TTYP are NOT in pull log',
      !output.includes(`DTEL ${OBJ_DTEL}`) && !output.includes(`TTYP ${OBJ_TTYP}`),
      `unexpected object in pull log: ${[`DTEL ${OBJ_DTEL}`, `TTYP ${OBJ_TTYP}`].filter(s => output.includes(s)).join(', ')}`);
  }
  printInfo('');

  // ─── Step 2: pull only DTEL ───────────────────────────────────────────────────
  printInfo(colorize('cyan', `Step 2: pull --files ${FILE_DTEL}  (DTEL only)`));
  {
    const { output, exitCode } = runPull(repoRoot, FILE_DTEL);

    addResult('[DTEL] pull succeeds',
      exitCode === 0 && output.includes('Pull completed'),
      `exitCode=${exitCode}\n  ${output.split('\n').find(l => l.includes('Error')) || ''}`);

    addResult('[DTEL] ZABGAGT_XO_DTEL processed by abapGit',
      output.includes(`DTEL ${OBJ_DTEL}`) || output.includes('object already active'),
      `"DTEL ${OBJ_DTEL}" not in pull log and no "already active" message`);

    addResult('[DTEL] TABL and TTYP are NOT in pull log',
      !output.includes(`TABL ${OBJ_TABL}`) && !output.includes(`TTYP ${OBJ_TTYP}`),
      `unexpected object in pull log: ${[`TABL ${OBJ_TABL}`, `TTYP ${OBJ_TTYP}`].filter(s => output.includes(s)).join(', ')}`);
  }
  printInfo('');

  // ─── Step 3: pull only TTYP ───────────────────────────────────────────────────
  printInfo(colorize('cyan', `Step 3: pull --files ${FILE_TTYP}  (TTYP only)`));
  {
    const { output, exitCode } = runPull(repoRoot, FILE_TTYP);

    addResult('[TTYP] pull succeeds',
      exitCode === 0 && output.includes('Pull completed'),
      `exitCode=${exitCode}\n  ${output.split('\n').find(l => l.includes('Error')) || ''}`);

    addResult('[TTYP] ZABGAGT_XO_TTYP processed by abapGit',
      output.includes(`TTYP ${OBJ_TTYP}`) || output.includes('object already active'),
      `"TTYP ${OBJ_TTYP}" not in pull log and no "already active" message`);

    addResult('[TTYP] TABL and DTEL are NOT in pull log',
      !output.includes(`TABL ${OBJ_TABL}`) && !output.includes(`DTEL ${OBJ_DTEL}`),
      `unexpected object in pull log: ${[`TABL ${OBJ_TABL}`, `DTEL ${OBJ_DTEL}`].filter(s => output.includes(s)).join(', ')}`);
  }
  printInfo('');

  // ─── Step 4: pull TABL + DTEL together ────────────────────────────────────────
  printInfo(colorize('cyan', `Step 4: pull --files ${FILE_TABL},${FILE_DTEL}  (TABL + DTEL)`));
  {
    const { output, exitCode } = runPull(repoRoot, [FILE_TABL, FILE_DTEL].join(','));

    addResult('[TABL+DTEL] pull succeeds',
      exitCode === 0 && output.includes('Pull completed'),
      `exitCode=${exitCode}\n  ${output.split('\n').find(l => l.includes('Error')) || ''}`);

    addResult('[TABL+DTEL] both TABL and DTEL processed by abapGit',
      (output.includes(`TABL ${OBJ_TABL}`) && output.includes(`DTEL ${OBJ_DTEL}`)) || output.includes('object already active'),
      `missing from pull log: ${[`TABL ${OBJ_TABL}`, `DTEL ${OBJ_DTEL}`].filter(s => !output.includes(s)).join(', ')}`);

    addResult('[TABL+DTEL] TTYP is NOT in pull log',
      !output.includes(`TTYP ${OBJ_TTYP}`),
      `"TTYP ${OBJ_TTYP}" unexpectedly appeared in pull log`);
  }
  printInfo('');

  // ─── Step 5: idempotency — re-pull TABL that is already active ────────────────
  printInfo(colorize('cyan', `Step 5: re-pull ${FILE_TABL} (idempotency — object already active)`));
  {
    const { output, exitCode } = runPull(repoRoot, FILE_TABL);

    addResult('[idempotency] re-pull of already-active TABL succeeds without error',
      exitCode === 0 && (output.includes('Pull completed') || output.includes('already active')),
      `exitCode=${exitCode}\n  ${output.split('\n').find(l => l.includes('Error') || l.includes('❌')) || ''}`);
  }
  printInfo('');

  // ─── Step 6: CLI rejection — .abapgit.xml has no valid object type segment ────
  // .abapgit.xml basename is ".abapgit.xml" — splits to ['', 'abapgit', 'xml'],
  // first part is empty → fails isXmlOnlyObject guard → rejected before any HTTP call.
  printInfo(colorize('cyan', 'Step 6: CLI rejects .abapgit.xml (invalid — empty name part)'));
  {
    const { output, exitCode } = runPull(repoRoot, 'src/.abapgit.xml');

    addResult('[CLI rejection] exits non-zero for .abapgit.xml',
      exitCode !== 0,
      `expected non-zero exit but got exitCode=${exitCode}`);

    addResult('[CLI rejection] error message mentions "not recognised"',
      output.includes('not recognised') || output.includes('not recognized'),
      `output: ${output.split('\n').find(l => l.includes('Error') || l.includes('❌')) || output.split('\n')[0]}`);
  }
  printInfo('');

  const duration    = ((Date.now() - startTime) / 1000).toFixed(1);
  const passedCount = results.filter(r => r.passed).length;
  const totalCount  = results.length;
  const success     = passedCount === totalCount;

  return { success, results, duration, passedCount, totalCount };
}

module.exports = { runXmlOnlyTests };
