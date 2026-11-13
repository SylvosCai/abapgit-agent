const path = require('path');
jest.mock('fs', () => ({
  readFileSync: jest.fn(),
  writeFileSync: jest.fn(),
  unlinkSync: jest.fn()
}));
jest.mock('child_process', () => ({ execSync: jest.fn() }));
const fs = require('fs');
const { execSync } = require('child_process');

const repoRoot = path.resolve(__dirname, '../..');
const draftPath = path.join(repoRoot, '.release-notes-draft.md');
const defaultCommand = 'pi --print --no-tools --no-session --no-context-files';
const notes = '## v1.2.4\n\n### Bug Fixes\n\n- Fix a user-facing issue\n\n---';
const stop = new Error('mocked process.exit');
let originalArgv;
let originalCommand;
let originalLegacyCommand;
let aiOutput;
let aiError;
let commits;

beforeEach(() => {
  originalArgv = process.argv;
  originalCommand = process.env.ABGAGT_RELEASE_NOTES_AI_COMMAND;
  originalLegacyCommand = process.env.RELEASE_NOTES_AI_COMMAND;
  delete process.env.ABGAGT_RELEASE_NOTES_AI_COMMAND;
  delete process.env.RELEASE_NOTES_AI_COMMAND;
  jest.clearAllMocks();
  jest.spyOn(console, 'log').mockImplementation(() => {});
  jest.spyOn(console, 'error').mockImplementation(() => {});
  jest.spyOn(process, 'exit').mockImplementation(() => { throw stop; });
  aiOutput = `\n${notes}\n`;
  aiError = null;
  commits = '---COMMIT---\nFix a user-facing issue\nCommit body with $(not-a-command)';
  fs.readFileSync.mockImplementation(file => {
    if (file === path.join(repoRoot, 'package.json')) return JSON.stringify({ version: '1.2.3' });
    throw new Error(`Unexpected read: ${file}`);
  });
  execSync.mockImplementation((command, options) => {
    if (command === 'git remote -v') return 'origin https://example.invalid/repo.git (fetch)\n';
    if (command === 'git tag --sort=-v:refname') return 'v1.2.3\n';
    if (command === 'git log v1.2.3..HEAD --format="---COMMIT---%n%s%n%b"') return commits;
    if (options && options.input) {
      if (aiError) throw aiError;
      return aiOutput;
    }
    throw new Error(`Unexpected subprocess: ${command}`);
  });
});

afterEach(() => {
  process.argv = originalArgv;
  for (const [key, value] of [
    ['ABGAGT_RELEASE_NOTES_AI_COMMAND', originalCommand],
    ['RELEASE_NOTES_AI_COMMAND', originalLegacyCommand]
  ]) {
    if (value === undefined) delete process.env[key];
    else process.env[key] = value;
  }
  jest.restoreAllMocks();
});

function runRelease(extraArgs = []) {
  process.argv = ['node', path.join(repoRoot, 'scripts/release.js'), '--patch', '--dry-run', ...extraArgs];
  expect(() => jest.isolateModules(() => require('../../scripts/release'))).toThrow(stop);
  expect(process.exit).toHaveBeenCalledWith(0);
  expect(execSync.mock.calls.some(([command]) => /^(npm version|git (add|commit|push|tag v))/.test(command))).toBe(false);
}

function aiCalls() {
  return execSync.mock.calls.filter(([, options]) => options && options.input);
}

test('uses Pi by default, forwards the prompt on stdin, and only writes the mocked draft', () => {
  runRelease(['--hint', 'Keep $(shell-text) literal']);
  expect(aiCalls()).toHaveLength(1);
  expect(aiCalls()[0]).toEqual([defaultCommand, expect.objectContaining({
    cwd: repoRoot,
    encoding: 'utf8',
    timeout: 120000,
    stdio: ['pipe', 'pipe', 'pipe'],
    input: expect.stringContaining('Generate concise release notes for version 1.2.4')
  })]);
  expect(aiCalls()[0][1].input).toContain('Commit body with $(not-a-command)');
  expect(aiCalls()[0][1].input).toContain('Keep $(shell-text) literal');
  expect(aiCalls()[0][0]).not.toContain('shell-text');
  expect(execSync.mock.calls.some(([command]) => command.startsWith('claude'))).toBe(false);
  expect(fs.writeFileSync).toHaveBeenCalledTimes(1);
  expect(fs.writeFileSync).toHaveBeenCalledWith(draftPath, notes);
  expect(fs.unlinkSync).toHaveBeenCalledWith(draftPath);
});

test.each(['', '   '])('uses Pi when the prefixed override is blank: %j', command => {
  process.env.ABGAGT_RELEASE_NOTES_AI_COMMAND = command;
  runRelease();
  expect(aiCalls()[0][0]).toBe(defaultCommand);
});

test('uses the prefixed custom command and ignores the old proposed variable', () => {
  process.env.ABGAGT_RELEASE_NOTES_AI_COMMAND = '  permitted-agent --print  ';
  process.env.RELEASE_NOTES_AI_COMMAND = 'claude --print';
  runRelease();
  expect(aiCalls()[0][0]).toBe('permitted-agent --print');
  expect(fs.writeFileSync).toHaveBeenCalledWith(draftPath, notes);
});

test('the old proposed variable cannot replace the Pi default', () => {
  process.env.RELEASE_NOTES_AI_COMMAND = 'claude --print';
  runRelease();
  expect(aiCalls()[0][0]).toBe(defaultCommand);
});

test.each(['ENOENT', 'ETIMEDOUT'])('keeps the existing fallback after AI failure: %s', code => {
  process.env.ABGAGT_RELEASE_NOTES_AI_COMMAND = 'permitted-agent --private-argument secret';
  aiError = Object.assign(new Error('Command failed: permitted-agent --private-argument secret'), { code });
  runRelease();
  expect(aiCalls()).toHaveLength(1);
  expect(fs.writeFileSync).toHaveBeenCalledWith(draftPath, '## v1.2.4\n\nSee commit history for changes.');
  expect(console.log).toHaveBeenCalledWith('AI CLI error:', code);
  expect(JSON.stringify(console.log.mock.calls)).not.toContain('private-argument');
});

test('does not invoke an AI when there are no commits', () => {
  commits = '';
  runRelease();
  expect(aiCalls()).toHaveLength(0);
  expect(fs.writeFileSync).toHaveBeenCalledWith(draftPath, '## v1.2.4\n\nSee commit history for changes.');
});
