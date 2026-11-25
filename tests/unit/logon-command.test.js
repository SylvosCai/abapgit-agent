const describe = process.platform === 'win32' ? global.describe.skip : global.describe;
const fs = require('node:fs');
const path = require('node:path');
const command = require('../../src/commands/logon');
const { fixture, system } = require('../helpers/central-catalog');
describe('central logon metadata', () => {
  let f, log, env;
  const raw = () => ({ system: 'DEV', client: '100', gitProfile: 'git', package: 'ZTEST', clientSettings: { '200': { transport: 'DEVK123456' } }, unrelated: 'preserve' });
  beforeEach(() => { f = fixture({ version: 1, systems: { DEV: system() } }); log = jest.spyOn(console, 'log').mockImplementation(() => {}); env = process.env.ABAP_LOGON_CLIENT; delete process.env.ABAP_LOGON_CLIENT; });
  afterEach(() => { if (env === undefined) delete process.env.ABAP_LOGON_CLIENT; else process.env.ABAP_LOGON_CLIENT = env; f.cleanup(); jest.restoreAllMocks(); });
  test('lists drafts without username/password leakage and reports override source', async () => {
    process.env.ABAP_LOGON_CLIENT = '200';
    await command.execute(['list', '--json'], { loadConfig: raw });
    expect(JSON.parse(log.mock.calls[0][0])).toEqual({ system: 'DEV', client: '200', source: 'environment', clients: [
      { client: '100', provider: 'file', selected: false }, { client: '200', provider: 'file', selected: true }
    ] });
    log.mockClear();
    await command.execute(['current', '--json'], { loadConfig: raw, logonClient: '100' });
    expect(JSON.parse(log.mock.calls[0][0])).toEqual({ system: 'DEV', client: '100', source: 'cli' });
  });
  test('use saves only the positional client, not the override; second use does not rewrite', async () => {
    jest.spyOn(process, 'cwd').mockReturnValue(f.dir);
    const file = path.join(f.dir, '.abapGitAgent');
    f.write(file, raw());
    process.env.ABAP_LOGON_CLIENT = '100';
    await command.execute(['use', '200', '--json'], { logonClient: '100' });
    expect(JSON.parse(fs.readFileSync(file, 'utf8'))).toEqual({ ...raw(), client: '200' });
    const ino = fs.statSync(file).ino;
    await command.execute(['use', '200', '--json'], {});
    expect(fs.statSync(file).ino).toBe(ino);
    expect(JSON.parse(log.mock.calls[1][0]).changed).toBe(false);
  });
  test('repositories persist independent defaults and never save temporary overrides', async () => {
    const dirs = ['a', 'b'].map(name => path.join(f.dir, name));
    dirs.forEach(dir => { fs.mkdirSync(dir, { mode: 0o700 }); f.write(path.join(dir, '.abapGitAgent'), raw()); });
    const cwd = jest.spyOn(process, 'cwd').mockReturnValue(dirs[0]);
    const loadConfig = () => JSON.parse(fs.readFileSync(path.join(process.cwd(), '.abapGitAgent'), 'utf8'));
    const catalog = fs.readFileSync(f.file, 'utf8');
    await command.execute(['use', '200', '--json'], {});
    cwd.mockReturnValue(dirs[1]);
    process.env.ABAP_LOGON_CLIENT = '200';
    await command.execute(['current', '--json'], { loadConfig });
    expect(JSON.parse(log.mock.calls.at(-1)[0])).toEqual({ system: 'DEV', client: '200', source: 'environment' });
    await command.execute(['current', '--json'], { loadConfig, logonClient: '100' });
    expect(JSON.parse(log.mock.calls.at(-1)[0]).source).toBe('cli');
    delete process.env.ABAP_LOGON_CLIENT;
    await command.execute(['current', '--json'], { loadConfig });
    expect(JSON.parse(log.mock.calls.at(-1)[0])).toEqual({ system: 'DEV', client: '100', source: 'repository' });
    cwd.mockReturnValue(dirs[0]);
    await command.execute(['current', '--json'], { loadConfig });
    expect(JSON.parse(log.mock.calls.at(-1)[0])).toEqual({ system: 'DEV', client: '200', source: 'repository' });
    expect(fs.readFileSync(f.file, 'utf8')).toBe(catalog);
  });
  test('use never writes a fallback config', async () => {
    jest.spyOn(process, 'cwd').mockReturnValue(f.dir);
    await expect(command.execute(['use', '200'], { loadConfig: raw })).rejects.toThrow();
    expect(fs.existsSync(path.join(f.dir, '.abapGitAgent'))).toBe(false);
  });
  test('explicit creation target works outside a repo and does not select the new client', async () => {
    const loadConfig = jest.fn(() => { throw new Error('must not read repo'); });
    await command.execute(['create', '003', '--system', 'DEV', '--provider', 'file', '--user', 'charlie', '--json'], { loadConfig });
    expect(loadConfig).not.toHaveBeenCalled();
    expect(f.read().systems.DEV.clients['003']).toEqual({ credentials: { provider: 'file', user: 'charlie', password: '' } });
    expect(JSON.parse(log.mock.calls[0][0])).toEqual({ system: 'DEV', client: '003', created: true, passwordRequired: true });
  });
  test('unknown override/legacy bindings/conflicting provider fields fail closed', async () => {
    await expect(command.execute(['current'], { loadConfig: raw, logonClient: '999' })).rejects.toThrow();
    await expect(command.execute(['current'], { loadConfig: () => ({ client: '100', user: 'legacy' }) })).rejects.toThrow();
    await expect(command.execute(['create', '300', '--system', 'DEV', '--provider', 'bitwarden', '--item-id', '11111111-1111-4111-8111-111111111111', '--adapter', 'bad'], {})).rejects.toThrow();
    expect(log).not.toHaveBeenCalled();
  });
});
