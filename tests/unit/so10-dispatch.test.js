jest.mock('../integration/so10-runner', () => ({ runSo10Tests: jest.fn() }));
jest.mock('../integration/setup-runner', () => ({ runSetup: jest.fn() }));

const { runSo10Tests } = require('../integration/so10-runner');
const { runSetup } = require('../integration/setup-runner');
const { main, runCommandTests, SUITES } = require('../run-all');
let argv;
let ci;

beforeEach(() => {
  argv = process.argv;
  ci = process.env.CI;
  jest.spyOn(console, 'log').mockImplementation(() => {});
  jest.spyOn(console, 'error').mockImplementation(() => {});
  jest.spyOn(process, 'exit').mockImplementation(() => {});
  runSo10Tests.mockReturnValue({ success: true, duration: '0.0', passedCount: 1, totalCount: 1, results: [{ passed: true }] });
  runSetup.mockImplementation(() => { throw new Error('Unexpected global ABAP setup'); });
});
afterEach(() => {
  process.argv = argv;
  if (ci === undefined) delete process.env.CI; else process.env.CI = ci;
  jest.restoreAllMocks();
  jest.clearAllMocks();
});

test('existing command selector dispatches SO10 runner instead of passing with zero cases', () => {
  const result = runCommandTests(false, 'so10');
  expect(result.totalCount).toBe(1);
  expect(runSo10Tests).toHaveBeenCalledTimes(1);
  expect(runSetup).not.toHaveBeenCalled();
});

test('explicit SO10 selection never invokes global clone/activation setup', async () => {
  process.argv = ['node', 'tests/run-all.js', '--cmd', '--command=so10'];
  await main();
  expect(runSo10Tests).toHaveBeenCalledTimes(1);
  expect(runSetup).not.toHaveBeenCalled();
  expect(process.exit).not.toHaveBeenCalled();
});

test('bare SO10 command filter also selects only its self-managed suite', async () => {
  process.argv = ['node', 'tests/run-all.js', '--command=so10'];
  await main();
  expect(runSo10Tests).toHaveBeenCalledTimes(1);
  expect(runSetup).not.toHaveBeenCalled();
  expect(process.exit).not.toHaveBeenCalled();
});

test('dedicated SO10 flag runs alone with its own summary and npm alias', async () => {
  expect(require('../../package.json').scripts['test:so10']).toBe('node tests/run-all.js --so10');
  process.argv = ['node', 'tests/run-all.js', '--so10'];
  await main();
  expect(runSo10Tests).toHaveBeenCalledTimes(1);
  expect(runSetup).not.toHaveBeenCalled();
  expect(console.log.mock.calls.flat().join('')).toContain('SO10 Tests: 1/1 PASSED');
});

test('default CI runs the dedicated SO10 suite exactly once', async () => {
  process.env.CI = 'true';
  process.argv = ['node', 'tests/run-all.js'];
  runSetup.mockReturnValue({ success: true });
  const so10 = SUITES.find(s => s.key === 'so10');
  expect(so10.ciDefault).toBe(true);
  for (const suite of SUITES.filter(s => s !== so10)) {
    jest.spyOn(suite, 'run').mockReturnValue({ success: true, duration: '0.0', passedCount: 1, totalCount: 1 });
  }
  jest.spyOn(global, 'setTimeout').mockImplementation(callback => { callback(); return 0; });
  await main();
  expect(runSo10Tests).toHaveBeenCalledTimes(1);
  expect(console.log.mock.calls.flat().join('')).toContain('SO10 Tests: 1/1 PASSED');
  expect(process.exit).not.toHaveBeenCalled();
});

test('a failed named lifecycle fails the unified suite', async () => {
  runSo10Tests.mockReturnValue({ success: false, passedCount: 0, totalCount: 1, duration: '0.0', results: [{ passed: false }] });
  process.argv = ['node', 'tests/run-all.js', '--so10'];
  await main();
  expect(runSetup).not.toHaveBeenCalled();
  expect(process.exit).toHaveBeenCalledWith(1);
});

test('explicit general setup remains unchanged', async () => {
  runSetup.mockReturnValue({ success: true });
  process.argv = ['node', 'tests/run-all.js', '--setup'];
  await main();
  expect(runSetup).toHaveBeenCalledTimes(1);
  expect(runSo10Tests).not.toHaveBeenCalled();
  expect(process.exit).toHaveBeenCalledWith(0);
});
