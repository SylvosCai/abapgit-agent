const fs = require('node:fs');
const path = require('node:path');
const https = require('node:https');
const http = require('node:http');
const debug = require('../../src/commands/debug');
const version = require('../../src/utils/version-check');
const tls = {
  key: fs.readFileSync(path.join(__dirname, '../fixtures/central-credentials/localhost-key.pem')),
  cert: fs.readFileSync(path.join(__dirname, '../fixtures/central-credentials/localhost-cert.pem'))
};
const listen = server => new Promise(resolve => server.listen(0, '::1', resolve));
const close = server => { server.closeAllConnections(); return new Promise(resolve => server.close(resolve)); };

describe('central command paths preserve connection safety', () => {
  test.each([true, false])('debug attach retains verifiedTls=%s on its replacement agent', async verifiedTls => {
    const agents = [];
    class FixtureAdt {
      constructor() {
        this._axios = { defaults: { httpsAgent: new https.Agent({ rejectUnauthorized: verifiedTls }) } };
        agents.push(this._axios.defaults.httpsAgent);
      }
      clearSession() {}
      async fetchCsrfToken() {
        const agent = this._axios.defaults.httpsAgent;
        agents.push(agent);
        expect(agent.options.rejectUnauthorized).toBe(verifiedTls);
        throw new Error('fixture stop before debug writes');
      }
    }
    try {
      await expect(debug.execute(['attach', '--json'], { loadConfig: () => ({ verifiedTls }), AdtHttpClass: FixtureAdt })).rejects.toThrow('fixture stop');
    } finally { agents.forEach(agent => agent.destroy()); }
  });
  test('debug attach rejects untrusted TLS before transmitting credentials', async () => {
    let requests = 0;
    const server = https.createServer(tls, (req, res) => { requests++; res.end('{}'); });
    await listen(server);
    const agents = [];
    class FixtureAdt {
      constructor() { this._axios = { defaults: { httpsAgent: new https.Agent({ rejectUnauthorized: true }) } }; agents.push(this._axios.defaults.httpsAgent); }
      clearSession() {}
      fetchCsrfToken() {
        const agent = this._axios.defaults.httpsAgent; agents.push(agent);
        return new Promise((resolve, reject) => {
          const req = https.get({ hostname: '::1', port: server.address().port, agent, headers: { Authorization: 'Basic ZHVtbXk6ZHVtbXk=' } }, res => { res.resume(); res.on('end', resolve); });
          req.on('error', reject);
        });
      }
    }
    try {
      await expect(debug.execute(['attach', '--json'], { loadConfig: () => ({ verifiedTls: true }), AdtHttpClass: FixtureAdt })).rejects.toThrow(/certificate|self.signed/i);
      expect(requests).toBe(0);
    } finally { agents.forEach(agent => agent.destroy()); await close(server); }
  });
  test.each(['::1', '[::1]'])('native IPv6 compatibility probe reaches the intended endpoint for %s', async host => {
    let headers;
    const server = http.createServer((req, res) => { headers = req.headers; res.end(JSON.stringify({ version: version.getCliVersion() })); });
    await listen(server);
    try {
      const result = await version.checkCompatibility({ host, sapport: server.address().port, protocol: 'http', client: '100', user: 'dummy', password: 'dummy' });
      expect(result.compatible).toBe(true);
      expect(headers['sap-client']).toBe('100');
    } finally { await close(server); }
  });
  test.each(['::1', '[::1]'])('HTTP and ADT retain valid legacy IPv6 base URLs for %s', host => {
    const { AbapHttp } = require('../../src/utils/abap-http');
    const { AdtHttp } = require('../../src/utils/adt-http');
    for (const Http of [AbapHttp, AdtHttp]) {
      const client = new Http({ host, sapport: 443, client: '100', user: 'dummy', password: 'dummy' });
      expect(client._axios.defaults.baseURL).toBe('https://[::1]:443');
      client._axios.defaults.httpsAgent.destroy();
    }
  });
  test('IPv6 compatibility probe preserves central TLS verification', async () => {
    let requests = 0;
    const server = https.createServer(tls, (req, res) => { requests++; res.end('{}'); });
    await listen(server);
    try {
      const result = await version.checkCompatibility({ host: '::1', sapport: server.address().port, protocol: 'https', client: '100', user: 'dummy', password: 'dummy', verifiedTls: true });
      expect(result.compatible).toBe(false);
      expect(result.error).toMatch(/certificate|self.signed/i);
      expect(requests).toBe(0);
    } finally { await close(server); }
  });
  test.each([
    ['transport', ['release', '--number', 'DUMMYK900001']], ['debug', ['attach']],
    ['ref', ['--clone', 'https://example.test/ref.git']], ['guide', ['--migrate', '--yes']]
  ])('%s help exits before configuration or actions', async (name, args) => {
    const command = require(`../../src/commands/${name}`);
    const log = jest.spyOn(console, 'log').mockImplementation(() => {});
    const context = new Proxy({}, { get() { throw new Error('unexpected context access'); } });
    const migrate = name === 'guide' ? jest.spyOn(command, '_runMigrate').mockImplementation(() => { throw new Error('unexpected migration'); }) : null;
    try {
      await command.execute([...args, '--help'], context);
      expect(log).toHaveBeenCalled();
      if (migrate) expect(migrate).not.toHaveBeenCalled();
    } finally { jest.restoreAllMocks(); }
  });
});
