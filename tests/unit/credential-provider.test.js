const describe = process.platform === 'win32' ? global.describe.skip : global.describe;
const fs = require('node:fs');
const path = require('node:path');
const http = require('node:http');
const { fixture } = require('../helpers/central-catalog');
const { helper, serveUrl, bitwarden, result } = require('../../src/utils/credential-provider');
const id = '11111111-1111-4111-8111-111111111111';
describe('credential provider trust boundaries', () => {
  let f;
  beforeEach(() => { f = fixture(); });
  afterEach(() => f.cleanup());
  const execute = (f, code, args = []) => {
    const file = path.join(f.dir, 'helper.cjs'); fs.writeFileSync(file, code);
    return helper('fixture', { executable: process.execPath, args: [file, 'prefix'] }, args);
  };
  test('real helper uses literal arguments, inherited environment and closed stdin; preserves password bytes', async () => {
    await expect(execute(f, `process.stdin.on('end', () => process.stdout.write(JSON.stringify({user:process.argv.slice(2).join('|'), password:' bytes \\n'}))); process.stdin.resume();`, ['a b', '$HOME', '--flag'])).resolves.toEqual({ user: 'prefix|a b|$HOME|--flag', password: ' bytes \n' });
  });
  test.each([
    `console.error('private-marker'); process.exit(1)`,
    `console.log('private-marker')`,
    `process.stdout.write(Buffer.from([0xff]))`,
    `console.log('{"user":"alice","password":""}')`,
    `console.log('{"user":"alice","password":"private-marker"}'); console.log('{}')`,
    `process.stderr.write('private-marker'.repeat(10000)); console.log('{}')`
  ])('real helper rejects failure/invalid output without exposing it', async code => {
    try { await execute(f, code); throw new Error('unexpected success'); } catch (e) { expect(e.message).toMatch(/Credential adapter fixture:/); expect(e.message).not.toContain('private-marker'); }
  });
  test('real helper timeout terminates its process group, including a descendant holding output open', async () => {
    const pidFile = path.join(f.dir, 'descendant.pid');
    const code = `const {spawn}=require('node:child_process'); const fs=require('node:fs'); const c=spawn(process.execPath,['-e','setInterval(()=>{},1000)'],{stdio:'inherit'}); fs.writeFileSync(${JSON.stringify(pidFile)},String(c.pid)); setInterval(()=>{},1000);`;
    await expect(execute(f, code)).rejects.toThrow('timeout');
    const pid = Number(fs.readFileSync(pidFile, 'utf8'));
    // A killed child may briefly remain a zombie; ps state must not be runnable.
    const { execFileSync } = require('node:child_process');
    let state = '';
    try { state = execFileSync('/bin/ps', ['-o', 'stat=', '-p', String(pid)], { encoding: 'utf8' }).trim(); } catch {}
    expect(state === '' || state.startsWith('Z')).toBe(true);
  }, 20000);
  test.each(['http://127.0.0.1:8087', 'https://[::1]:8087', 'http://[::1]:0', 'http://[::1]:65536', 'http://[::1]:8087/status', 'http://user@[::1]:8087', 'http://[::1]:8087?x=1', 'http://[::1]:8087#x'])('rejects unsafe serve endpoint %s before requests', value => {
    expect(() => serveUrl(value)).toThrow();
  });
  test('validates provider result shape and preserves nonempty whitespace passwords', () => {
    expect(result({ user: 'alice', password: ' ' })).toEqual({ user: 'alice', password: ' ' });
    expect(() => result({ user: ' ', password: 'p' })).toThrow();
    expect(() => result({ user: 'a', password: 123 })).toThrow();
  });
});

describe('Bitwarden HTTP transport with a local fixture (not an actual vault)', () => {
  let server, endpoint, requests, reply;
  beforeEach(async () => {
    requests = []; reply = null;
    server = http.createServer((req, res) => {
      requests.push(req.url);
      if (reply) return reply(req, res);
      res.end(JSON.stringify({ success: true, data: req.url === '/status' ? { template: { status: 'unlocked' } } : { id, type: 1, login: { username: 'alice', password: ' bytes \n' } } }));
    });
    await new Promise((resolve, reject) => { server.once('error', reject); server.listen(0, '::1', resolve); });
    endpoint = serveUrl(`http://[::1]:${server.address().port}`);
  });
  afterEach(async () => { server.closeAllConnections(); await new Promise(resolve => server.close(resolve)); });
  test('requests only status and exact item, without session/environment inputs', async () => {
    await expect(bitwarden(id, endpoint)).resolves.toEqual({ user: 'alice', password: ' bytes \n' });
    expect(requests).toEqual(['/status', `/object/item/${id}`]);
  });
  test.each(['locked', 'redirect', 'overflow', 'malformed', 'wrong-item', 'note', 'failed-envelope'])('rejects %s with redacted diagnostics and no redirects', async failure => {
    reply = (req, res) => {
      if (failure === 'redirect') { res.writeHead(302, { Location: 'http://remote.invalid/private-marker' }); res.end('private-marker'); return; }
      if (failure === 'overflow') { res.end('private-marker'.repeat(10000)); return; }
      if (failure === 'malformed') { res.end('private-marker'); return; }
      const data = req.url === '/status' ? { template: { status: failure === 'locked' ? 'locked' : 'unlocked' } } : { id: failure === 'wrong-item' ? 'wrong' : id, type: failure === 'note' ? 2 : 1, login: { username: 'a', password: 'private-marker' } };
      res.end(JSON.stringify({ success: failure !== 'failed-envelope', data }));
    };
    await expect(bitwarden(id, endpoint)).rejects.toThrow('Bitwarden lookup failed');
    expect(requests.length).toBeLessThanOrEqual(2);
  });
  test('invalid UUID makes no HTTP request', async () => {
    await expect(bitwarden('../private-marker', endpoint)).rejects.toThrow('Bitwarden lookup failed');
    expect(requests).toEqual([]);
  });
});
