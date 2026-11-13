const { EventEmitter } = require('events');
jest.mock('child_process', () => ({ execSync: jest.fn(), execFileSync: jest.fn(), spawn: jest.fn(), spawnSync: jest.fn() }));
jest.mock('fs', () => ({ existsSync: jest.fn(() => true), readFileSync: jest.fn(), unlinkSync: jest.fn() }));
jest.mock('../integration/abap-commands', () => []);
jest.mock('../integration/test-repos', () => ({
  getTestRepoUrl: jest.fn(() => 'https://github.tools.sap/test-org/abgagt-pull-test.git')
}));
const cases = require('../integration/abap-commands');
const { spawnSync, spawn } = require('child_process');
const { runCommandTests, runDemoCommand, runDemoCommandTests } = require('../run-all');

function failedCommand(stdout, status = 1, extra = {}) {
  return { stdout, stderr: 'diagnostic stderr', status, ...extra };
}
function demoChild(stdout, stderr, code, signal) {
  const child = new EventEmitter();
  child.stdout = new EventEmitter();
  child.stderr = new EventEmitter();
  process.nextTick(() => {
    child.stdout.emit('data', Buffer.from(stdout));
    child.stderr.emit('data', Buffer.from(stderr));
    child.emit('close', code, signal);
  });
  return child;
}

beforeEach(() => {
  cases.length = 0;
  jest.clearAllMocks();
  jest.spyOn(console, 'log').mockImplementation(() => {});
  jest.spyOn(console, 'error').mockImplementation(() => {});
  jest.spyOn(process.stdout, 'write').mockImplementation(() => true);
  jest.spyOn(process.stderr, 'write').mockImplementation(() => true);
});
afterEach(() => jest.restoreAllMocks());

function addCase(verify, overrides = {}) {
  const testCase = { name: 'negative syntax', command: 'syntax', args: [], expectSuccess: false, verify, ...overrides };
  cases.push(testCase);
  return testCase;
}
const intendedDiagnostic = (stdout, { exitCode }) => {
  const data = JSON.parse(stdout);
  return exitCode === 1 && data.type === 'BDEF' && data.error === 'invalid source';
};
const diagnostic = JSON.stringify({ type: 'BDEF', error: 'invalid source' });

test('verifies expected nonzero results, including stdout and stderr context', () => {
  const verify = jest.fn(intendedDiagnostic);
  addCase(verify);
  spawnSync.mockReturnValue(failedCommand(diagnostic));
  expect(runCommandTests(false, 'syntax').success).toBe(true);
  expect(verify).toHaveBeenCalledWith(diagnostic, { exitCode: 1, stderr: 'diagnostic stderr' });
});

test.each([
  ['authentication failure', '', 1, {}],
  ['malformed JSON', '{', 1, {}],
  ['wrong object identity', JSON.stringify({ type: 'SRVD', error: 'invalid source' }), 1, {}],
  ['wrong exit status', diagnostic, 2, {}],
  ['timeout', diagnostic, null, { signal: 'SIGTERM', code: 'ETIMEDOUT' }],
  ['spawn failure', diagnostic, undefined, { status: undefined, error: new Error('spawn ENOENT') }]
])('does not pass %s as an expected diagnostic', (_name, stdout, status, extra) => {
  addCase(intendedDiagnostic);
  spawnSync.mockReturnValue(failedCommand(stdout, status, extra));
  expect(runCommandTests(false, 'syntax').success).toBe(false);
});

test('preserves legacy semantic failures with exit zero and one-argument verifiers', () => {
  addCase(stdout => stdout.includes('semantic failure'));
  spawnSync.mockReturnValue({ stdout: 'semantic failure', status: 0 });
  expect(runCommandTests(false, 'syntax').success).toBe(true);
});

test('captures stderr on successful execution too', () => {
  const verify = jest.fn(() => true);
  addCase(verify, { expectSuccess: true });
  spawnSync.mockReturnValue({ stdout: 'valid', stderr: 'warning', status: 0 });
  expect(runCommandTests(false, 'syntax').success).toBe(true);
  expect(verify).toHaveBeenCalledWith('valid', { exitCode: 0, stderr: 'warning' });
});

test('cleanup failure makes a verified result fail', () => {
  addCase(() => true, { cleanup: () => { throw new Error('cannot remove input'); } });
  spawnSync.mockReturnValue({ stdout: 'valid', status: 0 });
  const result = runCommandTests(false, 'syntax');
  expect(result.success).toBe(false);
  expect(result.results[0].output).toContain('Cleanup failed: cannot remove input');
});

test('setup failure cannot pass a negative test and still cleans up', () => {
  const cleanup = jest.fn();
  addCase(() => true, { setup: () => { throw new Error('missing fixture'); }, cleanup });
  expect(runCommandTests(false, 'syntax').success).toBe(false);
  expect(spawnSync).not.toHaveBeenCalled();
  expect(cleanup).toHaveBeenCalledTimes(1);
});

test('demo verifies a nonzero result without mixing stderr into JSON', async () => {
  spawn.mockImplementation(() => demoChild(diagnostic, 'warning', 1));
  const verify = jest.fn(intendedDiagnostic);
  const result = await runDemoCommand('syntax --json', 'negative', verify, false);
  expect(result.passed).toBe(true);
  expect(verify).toHaveBeenCalledWith(diagnostic, { exitCode: 1, stderr: 'warning' });
});

test('demo rejects terminated processes even if their output looks valid', async () => {
  spawn.mockImplementation(() => demoChild(diagnostic, '', null, 'SIGTERM'));
  const verify = jest.fn(() => true);
  expect((await runDemoCommand('syntax', 'timeout', verify, false)).passed).toBe(false);
  expect(verify).not.toHaveBeenCalled();
});

test('demo reports spawn errors once without verifying their output', async () => {
  spawn.mockImplementation(() => {
    const child = new EventEmitter();
    child.stdout = new EventEmitter();
    child.stderr = new EventEmitter();
    process.nextTick(() => {
      child.emit('error', new Error('spawn ENOENT'));
      child.emit('close', -2);
    });
    return child;
  });
  const verify = jest.fn(() => true);
  const result = await runDemoCommand('syntax', 'spawn failure', verify, false);
  expect(result.passed).toBe(false);
  expect(result.output).toContain('spawn ENOENT');
  expect(verify).not.toHaveBeenCalled();
});

test('demo handles a verifier exception as failure', async () => {
  spawn.mockImplementation(() => demoChild('{', '', 1));
  expect((await runDemoCommand('syntax', 'malformed', intendedDiagnostic, false)).passed).toBe(false);
});

test('demo runs setup before assembling arguments and always runs cleanup', async () => {
  const testCase = addCase(intendedDiagnostic);
  testCase.setup = jest.fn(() => { testCase.args = ['--files', 'generated.bdef.asbdef']; });
  testCase.cleanup = jest.fn();
  spawn.mockImplementation(() => demoChild(diagnostic, '', 1));
  expect((await runDemoCommandTests(cases, Date.now())).success).toBe(true);
  expect(spawn.mock.calls[0][1]).toContain('generated.bdef.asbdef');
  expect(testCase.cleanup).toHaveBeenCalledTimes(1);
});

test('demo reports cleanup errors as failures', async () => {
  addCase(intendedDiagnostic, { cleanup: () => { throw new Error('cleanup failed'); } });
  spawn.mockImplementation(() => demoChild(diagnostic, '', 1));
  expect((await runDemoCommandTests(cases, Date.now())).success).toBe(false);
});

test('pull smoke test selects the configured fixture instead of downgrading the bridge', () => {
  const smoke = jest.requireActual('../integration/abap-commands').find(testCase => testCase.command === 'pull');
  expect(smoke.args).toEqual([
    '--url', 'https://github.tools.sap/test-org/abgagt-pull-test.git',
    '--files', 'src/zif_simple_test.intf.abap', '--branch', 'main'
  ]);
  expect(smoke.expectSuccess).toBe(true);
});

test.each([
  ['Pull completed successfully!', 0, true],
  ['Pull completed — object already active, nothing to activate.', 0, true],
  ['Pull completed successfully!', 1, false],
  ['Pull completed with errors!', 0, false],
  ['Failed Objects: ZIF_SIMPLE_TEST', 0, false],
  ['conflict detected', 0, false],
  ['Job ID: pending', 0, false],
  ['authentication failure', 1, false]
])('pull smoke test verifies completion and exit status: %s / %s', (stdout, exitCode, expected) => {
  const smoke = jest.requireActual('../integration/abap-commands').find(testCase => testCase.command === 'pull');
  expect(smoke.verify(stdout, { exitCode, stderr: '' })).toBe(expected);
});

const versionError = 'Error: Version 99.99.99 not found in npm registry\n'
  + '   Please check available versions at: https://www.npmjs.com/package/abapgit-agent?activeTab=versions';
test.each([
  ['', versionError, 1, true],
  [versionError, '', 1, true],
  ['', versionError, 0, false],
  ['', 'Authentication required', 1, false],
  ['', 'Version 99.99.99 not found in npm registry', 1, false],
  ['', versionError.replace('99.99.99', '88.88.88'), 1, false]
])('upgrade invalid-version verifier checks diagnostic, link and exit: %s / %s / %s', (stdout, stderr, exitCode, expected) => {
  const invalidVersion = jest.requireActual('../integration/abap-commands')
    .find(testCase => testCase.command === 'upgrade' && testCase.args.includes('99.99.99'));
  expect(invalidVersion.verify(stdout, { exitCode, stderr })).toBe(expected);
});
