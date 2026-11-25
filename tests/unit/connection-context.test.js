const describe = process.platform === 'win32' ? global.describe.skip : global.describe;
const { fixture, system } = require('../helpers/central-catalog');
const { createConnectionContext, selection, parseGlobal } = require('../../src/utils/connection-context');
const { transportProfile } = require('../../src/utils/so10-client-profile');
const { getScratchWorkspace } = require('../../src/config');
describe('pinned central connection context', () => {
  let f, env;
  beforeEach(() => {
    const dev = system(); dev.clients['200'].credentials.password = ' second secret ';
    f = fixture({ version: 1, systems: { DEV: dev }, gitProfiles: { work: { origin: 'https://git.example', credentials: { provider: 'file', user: 'git-user', password: ' git secret\n' } } } });
    env = process.env.ABAP_LOGON_CLIENT; delete process.env.ABAP_LOGON_CLIENT;
  });
  afterEach(() => { if (env === undefined) delete process.env.ABAP_LOGON_CLIENT; else process.env.ABAP_LOGON_CLIENT = env; f.cleanup(); });
  test('resolves a selected snapshot without changing raw repository settings', async () => {
    const raw = { system: 'DEV', client: '100', gitProfile: 'work', clientSettings: { '200': { transport: 'DEVK123', customizingClient: '100' } }, scratchWorkspace: { path: '/work' }, hooks: { transport: 'keep' } };
    const context = await createConnectionContext(raw, '200');
    expect(context.config).toMatchObject({ client: '200', user: 'bob', password: ' second secret ', protocol: 'https', sapport: 443, verifiedTls: true, transport: 'DEVK123', customizingClient: '100' });
    expect(context.config.gitPassword).toBeUndefined();
    expect(Object.isFrozen(context.config.clientSettings)).toBe(true);
    expect(raw.client).toBe('100'); expect(raw.password).toBeUndefined();
    expect(Object.isFrozen(raw.clientSettings)).toBe(false);
    expect(getScratchWorkspace(context.config).classPrefix).toBe('ZCL_BOB_');
    expect(context.loadConfig()).toBe(context.config);
  });
  test('catalog/provider results stay pinned across secondary logons and Git re-pulls', async () => {
    const context = await createConnectionContext({ system: 'DEV', client: '100', gitProfile: 'work', clientSettings: { '100': { customizingClient: '200', transport: 'DEVK1' } } });
    const changed = f.read(); changed.systems.DEV.clients['200'].credentials.password = 'changed'; changed.gitProfiles.work.credentials.password = 'changed'; f.write(f.file, changed);
    const secondary = await transportProfile(context.config, undefined, context.resolveSapClient);
    expect(secondary.password).toBe(' second secret '); expect(secondary.transport).toBeUndefined();
    expect(await context.resolveGitCredentials('https://git.example/repo')).toEqual({ username: 'git-user', password: ' git secret\n' });
    expect(await context.resolveGitCredentials('https://git.example/other')).toBe(await context.resolveGitCredentials('https://git.example/repo'));
    await expect(context.resolveSapClient('999')).rejects.toThrow();
  });
  test('secondary provider calls are memoized, including concurrent requests', async () => {
    const fs = require('node:fs'), path = require('node:path');
    const script = path.join(f.dir, 'secondary.cjs'), count = path.join(f.dir, 'calls');
    fs.writeFileSync(script, `const fs=require('node:fs'),file=${JSON.stringify(count)};const n=(fs.existsSync(file)?Number(fs.readFileSync(file,'utf8')):0)+1;fs.writeFileSync(file,String(n));console.log(JSON.stringify({user:'secondary',password:String(n)}));`);
    const changed = f.read();
    changed.cliAdapters = { fixture: { executable: process.execPath, args: [script] } };
    changed.systems.DEV.clients['200'].credentials = { provider: 'cli', adapter: 'fixture' };
    f.write(f.file, changed);
    const context = await createConnectionContext({ system: 'DEV', client: '100' });
    const [first, second] = await Promise.all([context.resolveSapClient('200'), context.resolveSapClient('200')]);
    expect(first).toBe(second); expect(first.password).toBe('1');
    expect(fs.readFileSync(count, 'utf8')).toBe('1');
    expect(await context.resolveSapClient('100')).toBe(context.config);
  });
  test('raw repository edits cannot retarget an invocation snapshot', async () => {
    const raw = { system: 'DEV', client: '100', gitProfile: 'work' };
    const context = await createConnectionContext(raw);
    raw.system = 'other'; raw.gitProfile = 'other';
    expect((await context.resolveSapClient('200')).host).toBe('sap.example');
    expect((await context.resolveGitCredentials('https://git.example/repo')).username).toBe('git-user');
  });
  test('Windows fails closed for either central binding without affecting legacy resolution', async () => {
    const descriptor = Object.getOwnPropertyDescriptor(process, 'platform');
    Object.defineProperty(process, 'platform', { ...descriptor, value: 'win32' });
    try {
      await expect(createConnectionContext({ system: 'DEV', client: '100' })).rejects.toThrow('POSIX');
      await expect(createConnectionContext({ gitProfile: 'work', host: 'sap.example' })).rejects.toThrow('POSIX');
      await expect(createConnectionContext({ host: 'sap.example', client: '100' })).resolves.toBeDefined();
    } finally { Object.defineProperty(process, 'platform', descriptor); }
  });
  test('Git-only migration is lazy for SAP-only commands but requires verified SAP TLS', async () => {
    const legacy = { host: 'sap.example', sapport: 443, client: '100', user: 'legacy', password: 'legacy-secret', gitProfile: 'work' };
    const context = await createConnectionContext(legacy);
    expect(context.config.verifiedTls).toBe(true);
    const changed = f.read(); changed.gitProfiles.work.credentials.password = 'first-needed'; f.write(f.file, changed);
    expect((await context.resolveGitCredentials('https://git.example/repo')).password).toBe('first-needed');
    await expect(createConnectionContext({ ...legacy, protocol: 'http' })).rejects.toThrow();
  });
  test.each(['https://other.example/repo', 'http://git.example/repo', 'https://u:p@git.example/repo', 'https://git.example:8443/repo', 'not a URL'])('Git destination %s never receives credentials', async url => {
    const context = await createConnectionContext({ gitProfile: 'work', protocol: 'https', host: 'sap.example' });
    await expect(context.resolveGitCredentials(url)).rejects.toThrow();
  });
  test('draft passwords and mixed central/flat bindings fail without fallback', async () => {
    const changed = f.read(); changed.systems.DEV.clients['100'].credentials.password = ''; f.write(f.file, changed);
    await expect(createConnectionContext({ system: 'DEV', client: '100' })).rejects.toThrow('invalid result');
    await expect(createConnectionContext({ system: 'DEV', client: '100', password: 'fallback' })).rejects.toThrow('conflicts');
    await expect(createConnectionContext({ gitProfile: 'work', gitPassword: 'fallback' })).rejects.toThrow('conflicts');
  });
  test('legacy fields and environment precedence remain distinct', async () => {
    process.env.ABAP_LOGON_CLIENT = '999';
    const raw = { host: 'sap.example', client: '100', user: 'legacy', password: 'legacy', gitUsername: 'git', gitPassword: 'token' };
    const context = await createConnectionContext(raw);
    expect(context.config.client).toBe('100'); expect(context.config.verifiedTls).toBeUndefined();
    expect(await context.resolveGitCredentials('http://legacy/repo')).toEqual({ username: 'git', password: 'token' });
    await expect(createConnectionContext(raw, '100')).rejects.toThrow();
    expect(selection({ system: 'DEV', client: '100' }, '200')).toEqual({ client: '200', source: 'cli' });
  });
  test('global parser removes only logon selectors and rejects repeats/missing/invalid values', () => {
    expect(parseGlobal(['--logon-client', '003', 'so10', '--target-client', '200'])).toEqual({ override: '003', args: ['so10', '--target-client', '200'] });
    expect(parseGlobal(['status', '--logon-client=100']).override).toBe('100');
    for (const args of [['--logon-client'], ['--logon-client', '20'], ['--logon-client', '100', '--logon-client=200']]) expect(() => parseGlobal(args)).toThrow();
  });
});
