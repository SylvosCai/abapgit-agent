/**
 * Sync-XML Integration Test Runner
 * Tests the --sync-xml flag end-to-end against a real ABAP system.
 *
 * Test Repository: abgagt-pull-test (URL configured via testRepos.pull in .abapGitAgent)
 *   Branch: feature/sync-xml-test
 *     - src/zif_simple_test.intf.xml has an extra <CATEGORY>00</CATEGORY>
 *       field that the abapGit serializer does NOT emit, so the bytes always
 *       differ after a pull — this is the mismatch that --sync-xml must fix.
 *
 * Test sequence:
 *
 *   Reset:     git reset --hard origin/feature/sync-xml-test in the temp clone
 *
 *   Step 1:    pull --files → warning shown, local XML still hand-crafted
 *   Step 2:    pull --files --sync-xml → XML rewritten, commit amended, re-pulled
 *   Step 3:    pull --files → no warning (already in sync)
 *   Step 4:    pull --files --sync-xml when already in sync → no sync, no error
 *
 *   Restore:   git reset --hard + force-push to restore hand-crafted XML
 *
 *   Step 5:    pull (no --files) → warning shown (full pull, unfiltered path)
 *   Step 6:    pull (no --files) --sync-xml → XML rewritten, commit amended, re-pulled
 *   Step 7:    pull (no --files) → no warning (already in sync)
 *
 *   Restore:   git reset --hard + force-push to restore hand-crafted XML
 *
 * PART C: XML-only object (DTEL) — no .abap source file
 *   The DTEL fixture (zabgagt_xo_dtel.dtel.xml) has a hand-crafted <PARAMID></PARAMID>
 *   field the serializer never writes. restoreDtel() re-injects it and bumps DDTEXT
 *   with a timestamp so abapGit always deserializes and returns local_xml_files.
 *
 *   Restore:   restoreDtel() — re-injects PARAMID, bumps DDTEXT, force-pushes
 *
 *   Step 8:    pull --files DTEL_XML → warning shown, local XML still hand-crafted
 *   Step 9:    pull --files DTEL_XML --sync-xml → XML rewritten, commit amended, re-pulled
 *   Step 10:   pull --files DTEL_XML → no warning (already in sync)
 *
 *   Restore:   restoreDtel()
 */

const { execSync } = require('child_process');
const path = require('path');
const fs = require('fs');
const os = require('os');

const { getTestRepoUrl } = require('./test-repos');
const { gitExec } = require('./git-helpers');
const TEST_REPO_URL  = getTestRepoUrl('pull');
const TEST_BRANCH    = 'feature/sync-xml-test';
const TEST_FILE      = 'src/zif_simple_test.intf.abap';
const TEST_XML_FILE  = 'src/zif_simple_test.intf.xml';
const DTEL_XML_FILE  = 'src/zabgagt_xo_dtel.dtel.xml';

/**
 * Run a pull command against the test repo using --url (works from any cwd).
 * Pass files=true to include --files, files=false for full pull (no --files).
 * @param {string}   agentBin
 * @param {string}   cloneDir  cwd for git ops (process.cwd() inside the CLI)
 * @param {boolean}  withFiles include --files flag
 * @param {string[]} extraArgs additional flags (e.g. ['--sync-xml'])
 * @returns {{ output: string, exitCode: number }}
 */
function runPull(agentBin, cloneDir, withFiles, extraArgs = []) {
  const filesArgs = withFiles ? ['--files', TEST_FILE] : [];
  const cmd = [
    'node', agentBin,
    'pull',
    '--url',    TEST_REPO_URL,
    '--branch', TEST_BRANCH,
    ...filesArgs,
    ...extraArgs
  ].join(' ');

  try {
    const output = execSync(cmd, { cwd: cloneDir, encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'], timeout: 120000 });
    return { output, exitCode: 0 };
  } catch (err) {
    return { output: (err.stdout || '') + (err.stderr || ''), exitCode: err.status || 1 };
  }
}

/**
 * Restore the clone and remote branch to the hand-crafted XML state.
 * Re-injects CATEGORY into the local XML and bumps a timestamp comment in
 * the ABAP source so the restore commit is never empty relative to its parent.
 * Commits and force-pushes. Idempotent regardless of what's on the remote.
 */
function restoreBranch(cloneDir, printInfo, printError) {
  printInfo(` Restoring remote branch to hand-crafted XML...`);
  try {
    // 1. Re-inject CATEGORY field into XML (if not already present)
    const xmlPath = path.join(cloneDir, TEST_XML_FILE);
    let xml = fs.readFileSync(xmlPath, 'utf8');
    if (!xml.includes('<CATEGORY>')) {
      xml = xml.replace(
        '    <EXPOSURE>',
        '    <CATEGORY>00</CATEGORY>\n    <EXPOSURE>'
      );
      fs.writeFileSync(xmlPath, xml);
    }

    // 2. Bump timestamp comment in ABAP source so the commit is never empty.
    //    (Without this, amending HEAD to remove CATEGORY produces an empty
    //    diff vs HEAD~1 which git refuses.)
    const abapPath = path.join(cloneDir, TEST_FILE);
    let abap = fs.readFileSync(abapPath, 'utf8');
    // Replace or insert a timestamp line at the top
    const ts = `" sync-xml-test restore: ${new Date().toISOString()}`;
    abap = abap.replace(/^" sync-xml-test restore:.*\n/m, '');
    abap = ts + '\n' + abap;
    fs.writeFileSync(abapPath, abap);

    // 3. Stage and commit both files
    execSync(`git add ${TEST_XML_FILE} ${TEST_FILE}`, { cwd: cloneDir, encoding: 'utf8' });
    execSync('git commit -m "test: restore hand-crafted XML with CATEGORY for sync-xml test"',
             { cwd: cloneDir, encoding: 'utf8' });

    // 4. Force-push so the remote matches
    gitExec(`git push --force-with-lease origin ${TEST_BRANCH}`, { cwd: cloneDir });
    return true;
  } catch (e) {
    printError(`  ✗ restoreBranch failed: ${e.message.split('\n')[0]}`);
    return false;
  }
}

/**
 * Restore the DTEL XML to the hand-crafted state (with PARAMID field).
 * Also bumps DDTEXT with a timestamp so abapGit always deserializes the
 * object (forcing local_xml_files to be returned) and the commit is
 * never empty relative to its parent.
 */
function restoreDtel(cloneDir, printInfo, printError) {
  printInfo(` Restoring DTEL XML to hand-crafted state (with PARAMID)...`);
  try {
    const xmlPath = path.join(cloneDir, DTEL_XML_FILE);

    // Write the full hand-crafted XML unconditionally.
    // --sync-xml may have reduced the file to a minimal skeleton, so patching
    // in-place is unreliable — always start from the known template.
    // Bump DDTEXT with ms-precision timestamp so abapGit sees a content change
    // and deserializes → local_xml_files is populated → sync-xml diff fires.
    const ts = new Date().toISOString().replace('T', ' ').slice(0, 23);
    const xml = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DTEL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD04V>
    <ROLLNAME>ZABGAGT_XO_DTEL</ROLLNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <DDTEXT>XML-only DTEL ${ts}</DDTEXT>
    <REPTEXT>XML-only DTEL</REPTEXT>
    <SCRTEXT_S>XML DTEL</SCRTEXT_S>
    <SCRTEXT_M>XML-only DTEL</SCRTEXT_M>
    <SCRTEXT_L>XML-only test data element</SCRTEXT_L>
    <DTELMASTER>E</DTELMASTER>
    <PARAMID>BUK</PARAMID>
    <DATATYPE>CHAR</DATATYPE>
    <LENG>000010</LENG>
   </DD04V>
  </asx:values>
 </asx:abap>
</abapGit>
`;
    fs.writeFileSync(xmlPath, xml);

    // Stage, commit, and force-push
    gitExec(`git add ${DTEL_XML_FILE}`, { cwd: cloneDir });
    execSync('git commit -m "test: restore hand-crafted DTEL XML with PARAMID for sync-xml test"',
             { cwd: cloneDir, encoding: 'utf8' });
    gitExec(`git push --force-with-lease origin ${TEST_BRANCH}`, { cwd: cloneDir });
    return true;
  } catch (e) {
    printError(`  ✗ restoreDtel failed: ${e.message.split('\n')[0]}`);
    return false;
  }
}

/**
 * Run sync-xml integration tests.
 * @param {string} repoRoot - abapgit-agent repository root
 * @param {Object} printFunctions
 */
function runSyncXmlTests(repoRoot, { printSubHeader, printInfo, printSuccess, printError, colorize }) {
  printSubHeader('Running --sync-xml Integration Tests');

  const startTime = Date.now();
  const results   = [];
  const agentBin  = path.join(repoRoot, 'bin', 'abapgit-agent');
  const cloneDir  = path.join(os.tmpdir(), 'abgagt-sync-xml-test');

  printInfo(`Test repo:   ${TEST_REPO_URL}`);
  printInfo(`Branch:      ${TEST_BRANCH}`);
  printInfo(`Working dir: ${cloneDir}`);
  printInfo('');

  // Copy .abapGitAgent credentials from the main project into the clone
  const credentialsSource = path.join(repoRoot, '.abapGitAgent');
  if (!fs.existsSync(credentialsSource)) {
    printError('✗ .abapGitAgent not found in project root — cannot run sync-xml tests');
    return { success: false, results, duration: '0.0', passedCount: 0, totalCount: 0, skipped: true };
  }

  // Clone or reset the temp working directory
  try {
    // Detect whether the existing directory is a valid git repo before trying fetch.
    // It may exist as an empty dir or a non-repo leftover from a previous failed run.
    let needsClone = true;
    if (fs.existsSync(cloneDir)) {
      try {
        execSync('git rev-parse --git-dir', { cwd: cloneDir, encoding: 'utf8', stdio: 'pipe' });
        // Valid git repo — fetch and reset
        gitExec(`git fetch origin ${TEST_BRANCH}`,        { cwd: cloneDir });
        execSync(`git checkout ${TEST_BRANCH}`,            { cwd: cloneDir, encoding: 'utf8' });
        execSync(`git reset --hard origin/${TEST_BRANCH}`, { cwd: cloneDir, encoding: 'utf8' });
        printInfo('Reset existing clone to remote state');
        needsClone = false;
      } catch (_) {
        // Not a valid repo — remove and re-clone below
        printInfo('Existing directory is not a valid git repo — removing and re-cloning');
        fs.rmSync(cloneDir, { recursive: true, force: true });
      }
    }
    if (needsClone) {
      gitExec(`git clone --branch ${TEST_BRANCH} ${TEST_REPO_URL} ${cloneDir}`);
      printInfo('Cloned fresh copy');
    }
    fs.copyFileSync(credentialsSource, path.join(cloneDir, '.abapGitAgent'));
    // Ensure the hand-crafted XML is present before the first test step
    restoreBranch(cloneDir, printInfo, printError);
  } catch (e) {
    printError(`✗ Failed to prepare test clone: ${e.message}`);
    const errMsg = `setup failed: ${e.message.split('\n')[0]}`;
    results.push({ name: 'sync-xml test setup (clone/fetch)', passed: false });
    printError(`✗ sync-xml test setup (clone/fetch)\n  ${errMsg}`);
    return { success: false, results, duration: '0.0', passedCount: 0, totalCount: 1 };
  }

  const addResult = (name, passed, hint = '') => {
    results.push({ name, passed });
    if (passed) {
      printSuccess(`✓ ${name}`);
    } else {
      printError(`✗ ${name}${hint ? `\n  ${hint}` : ''}`);
    }
  };

  const xmlHasCategory = () => {
    const xmlPath = path.join(cloneDir, TEST_XML_FILE);
    return fs.existsSync(xmlPath) && fs.readFileSync(xmlPath, 'utf8').includes('<CATEGORY>');
  };

  // ════════════════════════════════════════════════════════════════════════════
  // PART A: pull --files
  // ════════════════════════════════════════════════════════════════════════════

  // ── Step 1: pull --files → warning, XML not yet rewritten ───────────────────
  printInfo(colorize('cyan', 'Step 1 [--files]: pull without --sync-xml — expect "differ" warning'));
  {
    const { output, exitCode } = runPull(agentBin, cloneDir, true);

    addResult('[--files] pull succeeds',
      exitCode === 0 && output.includes('Pull completed'),
      `exitCode=${exitCode}`);

    addResult('[--files] warning: XML file(s) differ from serializer output',
      output.includes('XML file(s) differ from serializer output'),
      output.split('\n').find(l => l.includes('differ') || l.includes('⚠')) || '(no warning)');

    addResult('[--files] warning lists the interface XML file',
      output.includes(TEST_XML_FILE),
      `${TEST_XML_FILE} not mentioned in output`);

    addResult('[--files] local XML still has hand-crafted CATEGORY (not yet rewritten)',
      xmlHasCategory(),
      'CATEGORY already missing — XML may have been rewritten prematurely');
  }
  printInfo('');

  // ── Step 2: pull --files --sync-xml → rewrite + amend + re-pull ─────────────
  printInfo(colorize('cyan', 'Step 2 [--files]: pull --sync-xml — expect rewrite + amend + re-pull'));
  {
    const shaBefore = execSync('git rev-parse HEAD', { cwd: cloneDir, encoding: 'utf8' }).trim();

    const { output, exitCode } = runPull(agentBin, cloneDir, true, ['--sync-xml']);

    if (exitCode !== 0 || !output.includes('Pull completed')) {
      // Log full output (up to 800 chars) to help diagnose failures
      printError(`  [Step 2 debug] exitCode=${exitCode} output:\n${output.substring(0, 800)}`);
    }

    addResult('[--files] --sync-xml pull succeeds',
      exitCode === 0 && output.includes('Pull completed'),
      `exitCode=${exitCode}\n  ${output.split('\n').find(l => l.includes('Error')) || ''}`);

    addResult('[--files] "Syncing N XML file(s)" message appears',
      output.includes('Syncing') && output.includes('XML file(s)'),
      output.split('\n').find(l => l.includes('Sync')) || '(no Syncing message)');

    addResult('[--files] "Synced … amended commit, re-pulled" message appears',
      output.includes('Synced') && output.includes('amended commit') && output.includes('re-pulled'),
      output.split('\n').find(l => l.includes('Synced')) || '(no Synced message)');

    addResult('[--files] local XML CATEGORY field removed (serializer output accepted)',
      !xmlHasCategory(),
      'CATEGORY field still present — XML was not rewritten');

    const shaAfter = execSync('git rev-parse HEAD', { cwd: cloneDir, encoding: 'utf8' }).trim();
    addResult('[--files] git commit was amended (SHA changed)',
      shaAfter !== shaBefore,
      `SHA before=${shaBefore.slice(0, 7)} after=${shaAfter.slice(0, 7)}`);

    const gitStatus = execSync('git status --porcelain', { cwd: cloneDir, encoding: 'utf8' }).trim();
    addResult('[--files] working tree is clean after sync',
      gitStatus === '',
      `git status: ${gitStatus}`);
  }
  printInfo('');

  // ── Step 3: pull --files again → no warning (already in sync) ───────────────
  printInfo(colorize('cyan', 'Step 3 [--files]: pull again — no warning (already in sync)'));
  {
    const { output, exitCode } = runPull(agentBin, cloneDir, true);

    addResult('[--files] pull succeeds',
      exitCode === 0 && output.includes('Pull completed'),
      `exitCode=${exitCode}`);

    addResult('[--files] no "differ" warning when already in sync',
      !output.includes('differ from serializer output'),
      output.split('\n').find(l => l.includes('differ')) || '');
  }
  printInfo('');

  // ── Step 4: pull --files --sync-xml when already in sync → no-op ────────────
  printInfo(colorize('cyan', 'Step 4 [--files]: pull --sync-xml when already in sync — no-op'));
  {
    const shaBefore = execSync('git rev-parse HEAD', { cwd: cloneDir, encoding: 'utf8' }).trim();

    const { output, exitCode } = runPull(agentBin, cloneDir, true, ['--sync-xml']);

    addResult('[--files] --sync-xml pull succeeds when already in sync',
      exitCode === 0 && output.includes('Pull completed'),
      `exitCode=${exitCode}`);

    addResult('[--files] no "Syncing" message when already in sync',
      !output.includes('Syncing') || !output.includes('XML file(s)'),
      output.split('\n').find(l => l.includes('Syncing')) || '');

    const shaAfter = execSync('git rev-parse HEAD', { cwd: cloneDir, encoding: 'utf8' }).trim();
    addResult('[--files] no commit amend when already in sync (SHA unchanged)',
      shaAfter === shaBefore,
      `SHA changed: before=${shaBefore.slice(0, 7)} after=${shaAfter.slice(0, 7)}`);
  }
  printInfo('');

  // Restore remote branch to hand-crafted XML before full-pull tests
  const restoredForPartB = restoreBranch(cloneDir, printInfo, printError);
  printInfo('');

  // ════════════════════════════════════════════════════════════════════════════
  // PART B: full pull (no --files)
  // ════════════════════════════════════════════════════════════════════════════

  if (!restoredForPartB) {
    ['[no --files] pull succeeds', '[no --files] warning: XML file(s) differ from serializer output',
     '[no --files] local XML still has hand-crafted CATEGORY (not yet rewritten)',
     '[no --files] --sync-xml pull succeeds', '[no --files] "Syncing N XML file(s)" message appears',
     '[no --files] "Synced … amended commit, re-pulled" message appears',
     '[no --files] local XML CATEGORY field removed', '[no --files] git commit was amended (SHA changed)',
     '[no --files] working tree is clean after sync', '[no --files] pull succeeds (step 7)',
     '[no --files] no "differ" warning when already in sync'].forEach(name =>
      addResult(name, false, 'restoreBranch failed (git server error)'));
    printInfo('');
  } else {
  printInfo(colorize('cyan', 'Step 5 [no --files]: pull without --sync-xml — expect "differ" warning'));
  {
    const { output, exitCode } = runPull(agentBin, cloneDir, false);

    addResult('[no --files] pull succeeds',
      exitCode === 0 && output.includes('Pull completed'),
      `exitCode=${exitCode}`);

    addResult('[no --files] warning: XML file(s) differ from serializer output',
      output.includes('XML file(s) differ from serializer output'),
      output.split('\n').find(l => l.includes('differ') || l.includes('⚠')) || '(no warning)');

    addResult('[no --files] local XML still has hand-crafted CATEGORY (not yet rewritten)',
      xmlHasCategory(),
      'CATEGORY already missing');
  }
  printInfo('');

  // ── Step 6: pull (no --files) --sync-xml → rewrite + amend + re-pull ────────
  printInfo(colorize('cyan', 'Step 6 [no --files]: pull --sync-xml — expect rewrite + amend + re-pull'));
  {
    const shaBefore = execSync('git rev-parse HEAD', { cwd: cloneDir, encoding: 'utf8' }).trim();

    const { output, exitCode } = runPull(agentBin, cloneDir, false, ['--sync-xml']);

    addResult('[no --files] --sync-xml pull succeeds',
      exitCode === 0 && output.includes('Pull completed'),
      `exitCode=${exitCode}\n  ${output.split('\n').find(l => l.includes('Error')) || ''}`);

    addResult('[no --files] "Syncing N XML file(s)" message appears',
      output.includes('Syncing') && output.includes('XML file(s)'),
      output.split('\n').find(l => l.includes('Sync')) || '(no Syncing message)');

    addResult('[no --files] "Synced … amended commit, re-pulled" message appears',
      output.includes('Synced') && output.includes('amended commit') && output.includes('re-pulled'),
      output.split('\n').find(l => l.includes('Synced')) || '(no Synced message)');

    addResult('[no --files] local XML CATEGORY field removed',
      !xmlHasCategory(),
      'CATEGORY field still present — XML was not rewritten');

    const shaAfter = execSync('git rev-parse HEAD', { cwd: cloneDir, encoding: 'utf8' }).trim();
    addResult('[no --files] git commit was amended (SHA changed)',
      shaAfter !== shaBefore,
      `SHA before=${shaBefore.slice(0, 7)} after=${shaAfter.slice(0, 7)}`);

    const gitStatus = execSync('git status --porcelain', { cwd: cloneDir, encoding: 'utf8' }).trim();
    addResult('[no --files] working tree is clean after sync',
      gitStatus === '',
      `git status: ${gitStatus}`);
  }
  printInfo('');

  // ── Step 7: pull (no --files) again → no warning ────────────────────────────
  printInfo(colorize('cyan', 'Step 7 [no --files]: pull again — no warning (already in sync)'));
  {
    const { output, exitCode } = runPull(agentBin, cloneDir, false);

    addResult('[no --files] pull succeeds',
      exitCode === 0 && output.includes('Pull completed'),
      `exitCode=${exitCode}`);

    addResult('[no --files] no "differ" warning when already in sync',
      !output.includes('differ from serializer output'),
      output.split('\n').find(l => l.includes('differ')) || '');
  }
  printInfo('');
  } // end if (restoredForPartB)

  // Final restore before Part C
  restoreBranch(cloneDir, printInfo, printError);

  // ════════════════════════════════════════════════════════════════════════════
  // PART C: XML-only object (DTEL) — no .abap source file
  // ════════════════════════════════════════════════════════════════════════════
  // The DTEL fixture has a hand-crafted <PARAMID></PARAMID> field that the
  // abapGit serializer never writes for a plain CHAR element. restoreDtel()
  // re-injects it and bumps DDTEXT with a timestamp so abapGit always
  // deserializes the object and returns local_xml_files on every run.

  const restoredForPartC = restoreDtel(cloneDir, printInfo, printError);
  printInfo('');

  // ── Step 8: pull DTEL --files → warning, local XML still hand-crafted ────────
  if (!restoredForPartC) {
    ['[DTEL] pull succeeds', '[DTEL] warning: XML file(s) differ from serializer output',
     '[DTEL] warning lists the DTEL XML file', '[DTEL] local XML still has hand-crafted PARAMID (not yet rewritten)',
     '[DTEL] --sync-xml pull succeeds', '[DTEL] "Syncing N XML file(s)" message appears',
     '[DTEL] "Synced … amended commit, re-pulled" message appears', '[DTEL] PARAMID field removed',
     '[DTEL] git commit was amended (SHA changed)', '[DTEL] working tree is clean after sync',
     '[DTEL] pull succeeds (step 10)', '[DTEL] no "differ" warning when already in sync'].forEach(name =>
      addResult(name, false, 'restoreDtel failed (git server error)'));
  } else {
  {
    const { output, exitCode } = runPull(agentBin, cloneDir, false, ['--files', DTEL_XML_FILE]);

    addResult('[DTEL] pull succeeds',
      exitCode === 0 && output.includes('Pull completed'),
      `exitCode=${exitCode}`);

    addResult('[DTEL] warning: XML file(s) differ from serializer output',
      output.includes('XML file(s) differ from serializer output'),
      output.split('\n').find(l => l.includes('differ') || l.includes('⚠')) || '(no warning)');

    addResult('[DTEL] warning lists the DTEL XML file',
      output.includes(DTEL_XML_FILE),
      `${DTEL_XML_FILE} not mentioned in output`);

    const dtelXmlHasParamId = () => {
      const p = path.join(cloneDir, DTEL_XML_FILE);
      return fs.existsSync(p) && fs.readFileSync(p, 'utf8').includes('<PARAMID>');
    };
    addResult('[DTEL] local XML still has hand-crafted PARAMID (not yet rewritten)',
      dtelXmlHasParamId(),
      'PARAMID already missing — XML may have been rewritten prematurely');
  }
  printInfo('');

  // ── Step 9: pull DTEL --files --sync-xml → rewrite + amend + re-pull ─────────
  // Re-run restoreDtel to bump DDTEXT with a fresh timestamp so abapGit sees
  // a content change and deserializes (returning local_xml_files). Without this,
  // step 8's activation leaves the DTEL in sync with the remote and step 9
  // would get "Activation cancelled" with no XML diff to sync.
  restoreDtel(cloneDir, printInfo, printError);
  printInfo('');
  printInfo(colorize('cyan', 'Step 9 [DTEL --files]: pull --sync-xml — expect rewrite + amend + re-pull'));
  {
    const shaBefore = execSync('git rev-parse HEAD', { cwd: cloneDir, encoding: 'utf8' }).trim();

    const { output, exitCode } = runPull(agentBin, cloneDir, false, ['--files', DTEL_XML_FILE, '--sync-xml']);

    addResult('[DTEL] --sync-xml pull succeeds',
      exitCode === 0 && output.includes('Pull completed'),
      `exitCode=${exitCode}\n  ${output.split('\n').find(l => l.includes('Error')) || ''}`);

    addResult('[DTEL] "Syncing N XML file(s)" message appears',
      output.includes('Syncing') && output.includes('XML file(s)'),
      output.split('\n').find(l => l.includes('Sync')) || '(no Syncing message)');

    addResult('[DTEL] "Synced … amended commit, re-pulled" message appears',
      output.includes('Synced') && output.includes('amended commit') && output.includes('re-pulled'),
      output.split('\n').find(l => l.includes('Synced')) || '(no Synced message)');

    addResult('[DTEL] local XML PARAMID field removed (serializer output accepted)',
      !fs.readFileSync(path.join(cloneDir, DTEL_XML_FILE), 'utf8').includes('<PARAMID>'),
      'PARAMID field still present — XML was not rewritten');

    const shaAfter = execSync('git rev-parse HEAD', { cwd: cloneDir, encoding: 'utf8' }).trim();
    addResult('[DTEL] git commit was amended (SHA changed)',
      shaAfter !== shaBefore,
      `SHA before=${shaBefore.slice(0, 7)} after=${shaAfter.slice(0, 7)}`);

    const gitStatus = execSync('git status --porcelain', { cwd: cloneDir, encoding: 'utf8' }).trim();
    addResult('[DTEL] working tree is clean after sync',
      gitStatus === '',
      `git status: ${gitStatus}`);
  }
  printInfo('');

  // ── Step 10: pull DTEL --files again → no warning (already in sync) ──────────
  printInfo(colorize('cyan', 'Step 10 [DTEL --files]: pull again — no warning (already in sync)'));
  {
    const { output, exitCode } = runPull(agentBin, cloneDir, false, ['--files', DTEL_XML_FILE]);

    addResult('[DTEL] pull succeeds',
      exitCode === 0 && output.includes('Pull completed'),
      `exitCode=${exitCode}`);

    addResult('[DTEL] no "differ" warning when already in sync',
      !output.includes('differ from serializer output'),
      output.split('\n').find(l => l.includes('differ')) || '');
  }
  printInfo('');
  } // end if (restoredForPartC)

  // Final restore of DTEL
  restoreDtel(cloneDir, printInfo, printError);

  const duration    = ((Date.now() - startTime) / 1000).toFixed(1);
  const passedCount = results.filter(r => r.passed).length;
  const totalCount  = results.length;
  const success     = passedCount === totalCount;

  return { success, results, duration, passedCount, totalCount };
}

module.exports = { runSyncXmlTests };
