const describe = process.platform === 'win32' ? global.describe.skip : global.describe;
const fs = require('node:fs');
const path = require('node:path');
const { spawnSync } = require('node:child_process');
const { fixture, system } = require('../helpers/central-catalog');
const bin = path.resolve(__dirname, '../../bin/abapgit-agent');
describe('real CLI offline catalog/bootstrap behavior', () => {
  let f, preload;
  beforeEach(() => {
    f = fixture({ version: 1, systems: { DEV: system() } });
    f.write(path.join(f.dir, '.abapGitAgent'), { system: 'DEV', client: '100' });
    preload = path.join(f.dir, 'offline.cjs');
    fs.writeFileSync(preload, `require('node:os').homedir=()=>__dirname; for(const name of ['node:http','node:https']){const m=require(name);m.get=m.request=()=>{throw new Error('unexpected network call')}} const cp=require('node:child_process'); for(const key of ['spawn','spawnSync','exec','execSync','execFile','execFileSync'])cp[key]=()=>{throw new Error('unexpected subprocess')};`);
  });
  afterEach(() => f.cleanup());
  const run = (args, extraEnv = {}) => spawnSync(process.execPath, ['--require', preload, bin, ...args], {
    cwd: f.dir, encoding: 'utf8', timeout: 15000,
    env: { ...process.env, ABAP_LOGON_CLIENT: '', NODE_OPTIONS: '', ...extraEnv }
  });
  test('metadata JSON is one object and does not probe SAP/vault/npm or run tools', () => {
    for (const args of [['system', 'list', '--json'], ['credential', 'adapters', '--json'], ['--logon-client', '200', 'logon', 'current', '--json']]) {
      const output = run(args);
      expect(output.status).toBe(0); expect(output.stderr).not.toMatch(/unexpected/);
      expect(() => JSON.parse(output.stdout)).not.toThrow();
      expect(output.stdout).not.toMatch(/alice|bob|password|private bytes/);
    }
  });
  test('SAP command help and missing catalog subcommands are offline even with broken config', () => {
    fs.writeFileSync(path.join(f.dir, '.abapGitAgent'), 'broken-private-marker'); fs.unlinkSync(f.file);
    for (const args of [['pull', '--help'], ['system'], ['logon'], ['credential']]) {
      const output = run(args); expect(output.status).toBe(0); expect(output.stdout).toMatch(/Usage:/); expect(output.stdout).not.toContain('private-marker');
    }
  });
  test.each(Object.keys(require('../../src/commands')))('%s help is offline with broken config in either help spelling', command => {
    fs.writeFileSync(path.join(f.dir, '.abapGitAgent'), 'broken-private-marker'); fs.unlinkSync(f.file);
    for (const flag of ['--help', '-h']) {
      const output = run([command, flag]);
      expect(output.status).toBe(0); expect(output.stdout).toMatch(/Usage:/);
      expect(output.stdout + output.stderr).not.toMatch(/private-marker|unexpected/);
    }
  });
  test.each([
    ['transport', 'release', '--number', 'DUMMYK900001'], ['debug', 'attach'],
    ['guide', '--migrate', '--yes'], ['ref', '--clone', 'https://example.test/ref.git'],
    ['hana', 'view', 'drop'], ['hana', 'table', 'preview'], ['hana', 'schema', 'list']
  ])('nested help remains offline: %j', (...args) => {
    fs.writeFileSync(path.join(f.dir, '.abapGitAgent'), 'broken-private-marker'); fs.unlinkSync(f.file);
    const output = run([...args, '--help']);
    expect(output.status).toBe(0); expect(output.stdout).toMatch(/Usage:/);
    expect(output.stdout + output.stderr).not.toMatch(/private-marker|unexpected/);
  });
  test('effective environment or explicit central config passes the connection gate', () => {
    fs.unlinkSync(path.join(f.dir, '.abapGitAgent'));
    const environment = run(['health'], { CONFIG_PATH: path.join(f.dir, 'missing.json'), ABAP_HOST: '127.0.0.1', ABAP_USER: 'dummy', ABAP_PASSWORD: 'dummy' });
    expect(environment.stdout + environment.stderr).toMatch(/unexpected network/);
    expect(environment.stdout + environment.stderr).not.toMatch(/not configured/);
    const explicit = path.join(f.dir, 'explicit.json');
    f.write(explicit, { system: 'DEV', client: '100' });
    const central = run(['health'], { CONFIG_PATH: explicit, ABAP_LOGON_CLIENT: '100' });
    expect(central.stdout + central.stderr).toMatch(/unexpected network/);
    expect(central.stdout + central.stderr).not.toMatch(/not configured|private bytes/);
    const incomplete = run(['health'], { CONFIG_PATH: path.join(f.dir, 'missing.json'), ABAP_HOST: '127.0.0.1', ABAP_USER: 'dummy', ABAP_PASSWORD: '' });
    expect(incomplete.stdout).toMatch(/not configured/);
    expect(incomplete.stderr).not.toMatch(/unexpected network/);
  });
  test('explicit global client selection does not mutate repository config', () => {
    const before = fs.readFileSync(path.join(f.dir, '.abapGitAgent'), 'utf8');
    const output = run(['--logon-client=003', 'logon', 'current', '--json'], { ABAP_LOGON_CLIENT: '100' });
    expect(output.status).not.toBe(0); expect(output.stdout).toBe('');
    expect(fs.readFileSync(path.join(f.dir, '.abapGitAgent'), 'utf8')).toBe(before);
    const valid = run(['logon', 'current', '--json'], { ABAP_LOGON_CLIENT: '200' });
    expect(JSON.parse(valid.stdout)).toEqual({ system: 'DEV', client: '200', source: 'environment' });
  });
  test('an unusable draft fails before any SAP call', () => {
    const output = run(['--logon-client', '200', 'health']);
    expect(output.status).not.toBe(0); expect(output.stderr).toMatch(/invalid result/); expect(output.stderr).not.toMatch(/unexpected network/);
  });
});
