'use strict';

const http = require('node:http');
const { spawn } = require('node:child_process');
const { TextDecoder } = require('node:util');
const { performance } = require('node:perf_hooks');
const schema = require('./central-catalog');
const LIMIT = 64 * 1024;
const DEADLINE = 15000;

function json(bytes) {
  return JSON.parse(new TextDecoder('utf-8', { fatal: true }).decode(bytes));
}
function result(value) {
  if (!schema.object(value) || typeof value.user !== 'string' || !value.user.trim() || typeof value.password !== 'string' || !value.password.length) throw new Error('Credential provider returned an invalid result');
  return { user: value.user, password: value.password };
}
function helper(name, registration, args) {
  if (!schema.available(name, registration)) return Promise.reject(new Error(`Credential adapter ${name}: unavailable or unsupported executable`));
  return new Promise((resolve, reject) => {
    let child, settled = false, bytes = 0;
    const output = [];
    // shortcut: helpers must keep descendants in their process group; use stronger isolation for daemonizing tools.
    const stop = () => { if (child?.pid) { try { process.kill(-child.pid, 'SIGKILL'); } catch {} } };
    const finish = (error, value) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      stop();
      if (error) reject(new Error(`Credential adapter ${name}: ${error}`));
      else resolve(value);
    };
    const timer = setTimeout(() => finish('timeout'), DEADLINE);
    try {
      child = spawn(registration.executable, [...schema.strings(registration.args), ...schema.strings(args)], {
        shell: false, detached: true, env: process.env, stdio: ['ignore', 'pipe', 'pipe']
      });
      for (const stream of [child.stdout, child.stderr]) {
        stream.on('error', () => finish('capture failed'));
        stream.on('data', chunk => {
          bytes += chunk.length;
          if (bytes > LIMIT) finish('output limit exceeded');
          else if (stream === child.stdout) output.push(chunk);
        });
      }
      child.on('error', () => finish('launch failed'));
      child.on('close', code => {
        if (settled) return;
        if (code !== 0) return finish('failed');
        try { finish(null, result(json(Buffer.concat(output)))); } catch { finish('invalid credential response'); }
      });
    } catch { finish('launch failed'); }
  });
}
function serveUrl(value = process.env.BW_SERVE_URL || 'http://[::1]:8087') {
  let url;
  try { url = new URL(value); } catch { throw new Error('Invalid Bitwarden local-serve endpoint'); }
  if (typeof value !== 'string' || !/^http:\/\/\[::1\](?::\d+)?\/?$/.test(value) || url.protocol !== 'http:' || url.hostname !== '[::1]' || Number(url.port || 80) < 1 || url.username || url.password || url.pathname !== '/' || url.search || url.hash) throw new Error('Bitwarden endpoint must be HTTP IPv6 loopback with a port only');
  return url.origin;
}
async function bitwarden(itemId, endpoint) {
  const started = performance.now();
  let bytes = 0;
  const get = pathname => new Promise((resolve, reject) => {
    const left = DEADLINE - (performance.now() - started);
    if (left <= 0) return reject(new Error('timeout'));
    const chunks = [];
    let response;
    const request = http.get(`${endpoint}${pathname}`, { agent: false }, res => {
      response = res;
      if (res.statusCode !== 200) { res.destroy(); request.destroy(); reject(new Error('HTTP failure')); return; }
      res.on('data', chunk => {
        bytes += chunk.length;
        if (bytes > LIMIT) request.destroy(new Error('output limit'));
        else chunks.push(chunk);
      });
      res.on('error', reject);
      res.on('aborted', () => reject(new Error('aborted')));
      res.on('end', () => {
        try {
          const body = json(Buffer.concat(chunks));
          if (body.success !== true || !schema.object(body.data)) throw new Error('Invalid envelope');
          resolve(body.data);
        } catch (error) { reject(error); }
      });
    });
    const timer = setTimeout(() => { response?.destroy(); request.destroy(new Error('timeout')); }, left);
    request.on('error', reject);
    request.on('close', () => clearTimeout(timer));
  });
  try {
    if (!schema.uuid(itemId)) throw new Error('Invalid UUID');
    endpoint = serveUrl(endpoint);
    const status = await get('/status');
    if (status.template?.status !== 'unlocked') throw new Error('Locked');
    const item = await get(`/object/item/${itemId}`);
    if (typeof item.id !== 'string' || item.id.toLowerCase() !== itemId.toLowerCase() || item.type !== 1 || item.deletedDate) throw new Error('Invalid Login item');
    return result({ user: item.login?.username, password: item.login?.password });
  } catch { throw new Error('Bitwarden lookup failed: unlock/start local serve and check the Login item'); }
}
function createProviderResolver(catalog) {
  let endpoint;
  return async function resolve(credentials) {
    schema.credentials(credentials, catalog);
    if (credentials.provider === 'file') return result(credentials);
    if (credentials.provider === 'cli') return helper(credentials.adapter, catalog.cliAdapters[credentials.adapter], credentials.args);
    endpoint ??= serveUrl();
    return bitwarden(credentials.itemId, endpoint);
  };
}
module.exports = { createProviderResolver, helper, serveUrl, bitwarden, result };
