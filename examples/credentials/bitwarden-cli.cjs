// User-managed example: an unlocked CLI session, not bw serve or automatic login.
const { spawnSync } = require('node:child_process');
const { isAbsolute } = require('node:path');
const { TextDecoder } = require('node:util');
const { performance } = require('node:perf_hooks');

try {
  const [executable, itemId, ...extra] = process.argv.slice(2);
  if (!executable || !isAbsolute(executable) || extra.length ||
      !/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(itemId || '')) {
    throw new Error('Invalid arguments');
  }
  if (!process.env.BW_SESSION) throw new Error('Missing session');

  const deadline = performance.now() + 12000;
  let remainingBytes = 64 * 1024;
  const lookup = args => {
    const timeout = Math.ceil(deadline - performance.now());
    if (timeout <= 0 || remainingBytes <= 0) throw new Error('Lookup limit');
    const result = spawnSync(executable, ['--nointeraction', '--raw', ...args], {
      env: process.env,
      shell: false,
      stdio: ['ignore', 'pipe', 'pipe'],
      timeout,
      killSignal: 'SIGKILL',
      maxBuffer: remainingBytes
    });
    if (result.error || result.status !== 0) throw new Error('CLI failed');
    const output = result.stdout || Buffer.alloc(0);
    remainingBytes -= output.length + (result.stderr?.length || 0);
    if (remainingBytes < 0) throw new Error('Output limit');
    return JSON.parse(new TextDecoder('utf-8', { fatal: true }).decode(output));
  };

  if (lookup(['status']).status !== 'unlocked') throw new Error('Locked vault');
  const item = lookup(['get', 'item', itemId]);
  if (typeof item.id !== 'string' || item.id.toLowerCase() !== itemId.toLowerCase() ||
      item.type !== 1 || item.deletedDate ||
      typeof item.login?.username !== 'string' || !item.login.username ||
      typeof item.login?.password !== 'string' || !item.login.password) {
    throw new Error('Invalid Login item');
  }
  process.stdout.write(JSON.stringify({ user: item.login.username, password: item.login.password }) + '\n');
} catch {
  // Never print raw CLI errors, session keys, lookup arguments or vault output.
  process.stderr.write('Bitwarden adapter failed. Check the CLI path, unlocked session and Login item.\n');
  process.exitCode = 1;
}
