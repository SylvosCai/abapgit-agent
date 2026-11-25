const describe = process.platform === 'win32' ? global.describe.skip : global.describe;
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const crypto = require('node:crypto');
const https = require('node:https');
const { fixture } = require('../helpers/central-catalog');
const state = require('../../src/utils/connection-state');
const { getDaemonSocketPath } = require('../../src/utils/debug-state');
const { AbapHttp } = require('../../src/utils/abap-http');
const { AdtHttp } = require('../../src/utils/adt-http');
const base = { host: '127.0.0.1', sapport: 443, protocol: 'https', client: '100', user: 'fixture-user', password: 'fixture-password', verifiedTls: true };
describe('central endpoint state and real local TLS fixture', () => {
  let f, server;
  beforeEach(() => { f = fixture(); jest.spyOn(os, 'homedir').mockReturnValue(f.dir); });
  afterEach(async () => {
    if (server) { server.closeAllConnections(); await new Promise(resolve => server.close(resolve)); server = null; }
    jest.restoreAllMocks(); f.cleanup();
  });
  test('full endpoint/client/user identities isolate session, breakpoint and socket locations', () => {
    const configs = [base, { ...base, sapport: 8443 }, { ...base, protocol: 'http' }, { ...base, client: '200' }, { ...base, user: 'other' }];
    expect(new Set(configs.map(state.key)).size).toBe(configs.length);
    expect(new Set(configs.map(getDaemonSocketPath)).size).toBe(configs.length);
    for (const prefix of ['abapgit-session', 'abapgit-debug', 'abapgit-breakpoints']) expect(new Set(configs.map(c => state.stateFile(c, prefix))).size).toBe(configs.length);
    expect(state.key(base)).toBe(state.key({ ...base, host: base.host.toUpperCase() }));
  });
  test('central session storage is private and old endpoint-incomplete cache keys are not reused', () => {
    const client = new AbapHttp(base);
    expect(client.csrfToken).toBeNull();
    client.csrfToken = 'fixture-csrf'; client.cookies = 'fixture-cookie=value'; client.saveSession();
    expect(fs.statSync(client.sessionFile).mode & 0o777).toBe(0o600);
    expect(fs.statSync(path.dirname(client.sessionFile)).mode & 0o777).toBe(0o700);
    expect(new AbapHttp(base).csrfToken).toBe('fixture-csrf');
    expect(new AbapHttp({ ...base, sapport: 8443 }).csrfToken).toBeNull();
    const old = crypto.createHash('md5').update(`${base.host}:${base.user}:${base.client}`).digest('hex').slice(0, 8);
    expect(client.sessionFile).not.toContain(`-${old}.json`);
    expect(client.sessionFile).toContain('-v2-');
  });
  test('existing public version-cache directory is preserved while connection state stays private', () => {
    const cache = path.join(f.dir, '.cache', 'abapgit-agent');
    fs.mkdirSync(cache, { recursive: true, mode: 0o755 }); fs.chmodSync(cache, 0o755);
    const versionFile = path.join(cache, 'version-check.json');
    fs.writeFileSync(versionFile, '{"version":"fixture"}');
    const client = new AbapHttp(base);
    client.csrfToken = 'fixture-token'; client.saveSession();
    expect(path.dirname(client.sessionFile)).toBe(path.join(cache, 'connections'));
    expect(fs.statSync(path.dirname(client.sessionFile)).mode & 0o777).toBe(0o700);
    expect(fs.statSync(client.sessionFile).mode & 0o777).toBe(0o600);
    expect(fs.statSync(cache).mode & 0o777).toBe(0o755);
    expect(fs.readFileSync(versionFile, 'utf8')).toBe('{"version":"fixture"}');
  });
  test.each([AbapHttp, AdtHttp])('central client rejects a self-signed server before HTTP auth; legacy behavior is unchanged', async Client => {
    let received = 0;
    const dir = path.resolve(__dirname, '../fixtures/central-credentials');
    server = https.createServer({ key: fs.readFileSync(path.join(dir, 'localhost-key.pem')), cert: fs.readFileSync(path.join(dir, 'localhost-cert.pem')) }, (req, res) => {
      received++; res.setHeader('Content-Type', 'application/json'); res.end('{}');
    });
    await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
    const config = { ...base, sapport: server.address().port };
    const central = new Client(config);
    // Bypass ambient proxies only for this deterministic loopback fixture.
    central._axios.defaults.proxy = false;
    await expect(central.get('/probe', { timeout: 2000 })).rejects.toThrow();
    expect(received).toBe(0);
    const legacy = new Client({ ...config, verifiedTls: false });
    legacy._axios.defaults.proxy = false;
    await expect(legacy.get('/probe', { timeout: 2000 })).resolves.toBeDefined();
    expect(received).toBe(1);
  });
});
