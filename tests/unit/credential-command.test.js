const describe = process.platform === 'win32' ? global.describe.skip : global.describe;
const fs = require('node:fs');
const command = require('../../src/commands/credential');
const { fixture } = require('../helpers/central-catalog');
describe('central credential metadata', () => {
  let f, log;
  beforeEach(() => { f = fixture(); log = jest.spyOn(console, 'log').mockImplementation(() => {}); });
  afterEach(() => { f.cleanup(); jest.restoreAllMocks(); });
  test('registers absolute helpers, preserves literal args and lists no paths', async () => {
    await command.execute(['register', 'company', '--executable', process.execPath, '--arg', '/nonexistent/helper.js', '--arg=--prefix', '--json']);
    expect(f.read().cliAdapters.company.args).toEqual(['/nonexistent/helper.js', '--prefix']);
    log.mockClear();
    await command.execute(['adapters', '--json']);
    expect(JSON.parse(log.mock.calls[0][0])).toEqual({ adapters: [{ name: 'company', source: 'custom', available: false }] });
  });
  test('creates file drafts without accepting or outputting a secret', async () => {
    await command.execute(['create', 'git', '--origin', 'https://GIT.example:443', '--provider', 'file', '--user', 'alice', '--json']);
    expect(f.read().gitProfiles.git).toEqual({ origin: 'https://git.example', credentials: { provider: 'file', user: 'alice', password: '' } });
    expect(JSON.parse(log.mock.calls[0][0])).toEqual({ gitProfile: 'git', created: true, passwordRequired: true });
    await expect(command.execute(['create', 'other', '--origin', 'https://git.example', '--provider', 'file', '--user', 'alice', '--password', 'secret'])).rejects.toThrow();
  });
  test.each(['http://git.example', 'https://alice:secret@git.example', 'https://git.example/path', 'https://git.example?q=a', 'https://git.example#frag'])('rejects non-origin %s', async origin => {
    await expect(command.execute(['create', 'git', '--origin', origin, '--provider', 'file', '--user', 'alice'])).rejects.toThrow();
    expect(f.read().gitProfiles).toBeUndefined();
  });
  test.each(['file', 'bitwarden', 'cli', 'BadName'])('rejects reserved/invalid adapter %s', async name => {
    await expect(command.execute(['register', name, '--executable', '/missing'])).rejects.toThrow();
  });
  test('explicit missing catalog errors on listing but register initializes it', async () => {
    fs.unlinkSync(f.file);
    await expect(command.execute(['adapters'])).rejects.toThrow();
    await command.execute(['register', 'company', '--executable', '/not-installed']);
    expect(f.read().cliAdapters.company.executable).toBe('/not-installed');
  });
  test('metadata operations do not validate unrelated secrets', async () => {
    f.write(f.file, { version: 1, systems: { broken: 'private-marker' }, gitProfiles: { broken: { credentials: 'private-marker' } } });
    await command.execute(['adapters', '--json']);
    expect(log).toHaveBeenCalledWith('{"adapters":[]}');
  });
});
