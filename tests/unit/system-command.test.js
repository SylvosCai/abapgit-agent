const describe = process.platform === 'win32' ? global.describe.skip : global.describe;
const fs = require('node:fs');
const command = require('../../src/commands/system');
const { fixture, system } = require('../helpers/central-catalog');
describe('central system metadata', () => {
  let f, log;
  beforeEach(() => { f = fixture({ version: 1, systems: { DEV: system() }, unrelated: 'private-marker' }); log = jest.spyOn(console, 'log').mockImplementation(() => {}); });
  afterEach(() => { f.cleanup(); jest.restoreAllMocks(); });
  test('lists only endpoint metadata with defaults', async () => {
    await command.execute(['list', '--json']);
    expect(JSON.parse(log.mock.calls[0][0])).toEqual({ systems: [{ name: 'DEV', protocol: 'https', host: 'sap.example', sapport: 443, language: 'EN' }] });
    expect(log.mock.calls.flat().join()).not.toMatch(/alice|private|password/);
  });
  test('creates one entry preserving unrelated data and private permissions', async () => {
    await command.execute(['create', 'QA', '--host', '127.0.0.1', '--port', '8443', '--json']);
    expect(f.read().systems.QA).toEqual({ protocol: 'https', host: '127.0.0.1', sapport: 8443, language: 'EN', clients: {} });
    expect(f.read().unrelated).toBe('private-marker');
    expect(fs.statSync(f.file).mode & 0o777).toBe(0o600);
  });
  test.each([['create', 'DEV', '--host', 'sap.example'], ['create', 'QA', '--host', 'https://sap.example'], ['create', 'QA', '--host', 'sap..example'], ['create', 'QA', '--host', 'sap.example', '--port', '0'], ['list', '--password', 'secret']])('rejects invalid creation/options: %j', async (...args) => {
    const before = fs.readFileSync(f.file, 'utf8');
    await expect(command.execute(args)).rejects.toThrow();
    expect(fs.readFileSync(f.file, 'utf8')).toBe(before);
    expect(log).not.toHaveBeenCalled();
  });
  test('missing catalogs are created only by create; help reads nothing', async () => {
    fs.unlinkSync(f.file);
    await command.execute(['--help']);
    await expect(command.execute(['list'])).rejects.toThrow();
    await command.execute(['create', 'DEV', '--host', 'sap.example']);
    expect(f.read().systems.DEV.clients).toEqual({});
  });
});
