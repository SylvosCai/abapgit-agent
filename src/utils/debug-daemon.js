'use strict';

/**
 * debug-daemon.js — Long-lived daemon that holds the stateful ADT HTTP connection.
 *
 * Why this exists:
 *   SAP ADT debug sessions are pinned to a specific ABAP work process via the
 *   SAP_SESSIONID cookie. The cookie is maintained in-memory by AdtHttp. When a
 *   CLI process exits (e.g. `debug attach --json`) the in-memory session is lost
 *   and the ABAP work process is released from the debugger. Subsequent CLI
 *   invocations (`debug step`) get a different HTTP connection → `noSessionAttached`.
 *
 *   The daemon keeps one Node.js process alive after attach, holding the open
 *   HTTP session. Individual CLI commands connect via Unix domain socket, send a
 *   JSON command, read a JSON response, and exit.
 *
 * Lifecycle:
 *   1. Spawned by `debug attach --json` with detached:true + stdio:ignore + unref()
 *   2. Reads config/session from env vars (JSON-encoded)
 *   3. Creates Unix socket at DEBUG_DAEMON_SOCK_PATH
 *   4. Handles JSON-line commands until `terminate` or 30-min idle timeout
 *   5. Deletes socket file and exits
 *
 * IPC Protocol — newline-delimited JSON over Unix socket:
 *
 *   Commands:
 *     { "cmd": "ping" }
 *     { "cmd": "step",      "type": "stepOver|stepInto|stepReturn|stepContinue" }
 *     { "cmd": "vars",      "name": null }
 *     { "cmd": "stack" }
 *     { "cmd": "terminate" }
 *
 *   Responses (one JSON line per command):
 *     { "ok": true,  "pong": true }
 *     { "ok": true,  "position": {...}, "source": [...] }
 *     { "ok": true,  "variables": [...] }
 *     { "ok": true,  "frames": [...] }
 *     { "ok": true,  "terminated": true }
 *     { "ok": false, "error": "message", "statusCode": 400 }
 */

const net = require('net');
const fs  = require('fs');
const { AdtHttp }      = require('./adt-http');
const { DebugSession } = require('./debug-session');

const DAEMON_IDLE_TIMEOUT_MS = 30 * 60 * 1000; // 30 minutes

// ─── Public API ───────────────────────────────────────────────────────────────

/**
 * Start the daemon server.
 *
 * @param {object} config           - ABAP connection config
 * @param {string} sessionId        - debugSessionId returned by attach
 * @param {string} socketPath       - Unix socket path to listen on
 * @param {object|null} snapshot    - { csrfToken, cookies } captured from attach process
 */
async function startDaemon(config, sessionId, socketPath, snapshot) {
  const adt = new AdtHttp(config);

  // Restore the exact SAP_SESSIONID cookie + CSRF token from the attach process.
  // This guarantees the first IPC command reuses the same ABAP work process
  // without another round-trip for a new token.
  if (snapshot && snapshot.csrfToken) adt.csrfToken = snapshot.csrfToken;
  if (snapshot && snapshot.cookies)   adt.cookies   = snapshot.cookies;

  const session = new DebugSession(adt, sessionId);

  // Pin the SAP_SESSIONID so _restorePinnedSession() works inside the daemon.
  // DebugSession.attach() normally sets pinnedSessionId, but the daemon skips
  // attach() and reconstructs the session from the snapshot.  Without this,
  // any CSRF refresh that AdtHttp performs internally (401/403 retry → HEAD
  // request → new Set-Cookie) silently overwrites the session cookie and routes
  // subsequent IPC calls to the wrong ABAP work process → HTTP 400.
  if (snapshot && snapshot.cookies) {
    const m = snapshot.cookies.match(/SAP_SESSIONID=([^;]*)/);
    if (m) session.pinnedSessionId = m[1];
  }

  // Remove stale socket file from a previous crash
  try { fs.unlinkSync(socketPath); } catch (e) { /* ignore ENOENT */ }

  let idleTimer = null;

  function resetIdle() {
    if (idleTimer) clearTimeout(idleTimer);
    idleTimer = setTimeout(cleanupAndExit, DAEMON_IDLE_TIMEOUT_MS);
    // unref so idle timer alone doesn't keep the process alive — if the
    // server stops listening for another reason the process can exit naturally
    if (idleTimer.unref) idleTimer.unref();
  }

  function cleanupAndExit(code) {
    try { fs.unlinkSync(socketPath); } catch (e) { /* ignore */ }
    process.exit(code || 0);
  }

  // On SIGTERM (e.g. pkill from ensure_breakpoint cleanup), attempt to release
  // the frozen ABAP work process before exiting.  Without this, killing the
  // daemon leaves the work process paused at the breakpoint until SAP's own
  // session-timeout fires (up to several minutes).
  process.once('SIGTERM', async () => {
    try {
      await session.terminate();
    } catch (e) { /* ignore — best effort */ }
    cleanupAndExit(0);
  });

  const server = net.createServer((socket) => {
    resetIdle();
    let buf = '';

    socket.on('data', (chunk) => {
      buf += chunk.toString();
      let idx;
      while ((idx = buf.indexOf('\n')) !== -1) {
        const line = buf.slice(0, idx).trim();
        buf = buf.slice(idx + 1);
        if (line) {
          _handleLine(socket, line, session, cleanupAndExit, resetIdle);
        }
      }
    });

    socket.on('error', () => { /* client disconnected abruptly — ignore */ });
  });

  server.listen(socketPath, () => {
    resetIdle();
  });

  server.on('error', (err) => {
    process.stderr.write(`[debug-daemon] server error: ${err.message}\n`);
    cleanupAndExit(1);
  });
}

/**
 * Handle one parsed JSON command line.
 * Exported so unit tests can call it directly without spawning a real server.
 */
async function _handleLine(socket, line, session, cleanupAndExit, resetIdle) {
  let req;
  try {
    req = JSON.parse(line);
  } catch (e) {
    _send(socket, { ok: false, error: `Invalid JSON: ${e.message}` });
    return;
  }

  resetIdle();

  try {
    switch (req.cmd) {
      case 'ping': {
        _send(socket, { ok: true, pong: true });
        break;
      }
      case 'step': {
        const result = await session.step(req.type || 'stepOver');
        _send(socket, { ok: true, position: result.position, source: result.source });
        break;
      }
      case 'vars': {
        const variables = await session.getVariables(req.name || null);
        _send(socket, { ok: true, variables });
        break;
      }
      case 'expand': {
        const children = await session.getVariableChildren(req.id, req.meta || {});
        _send(socket, { ok: true, variables: children });
        break;
      }
      case 'stack': {
        const frames = await session.getStack();
        _send(socket, { ok: true, frames });
        break;
      }
      case 'terminate': {
        await session.terminate();
        _send(socket, { ok: true, terminated: true });
        // Flush response before exiting — wait for client to close the socket
        socket.end(() => cleanupAndExit(0));
        break;
      }
      default: {
        _send(socket, { ok: false, error: `Unknown command: ${req.cmd}` });
      }
    }
  } catch (err) {
    _send(socket, {
      ok: false,
      error: err.message || JSON.stringify(err),
      statusCode: err.statusCode,
      body: err.body || null
    });
  }
}

function _send(socket, obj) {
  try {
    socket.write(JSON.stringify(obj) + '\n');
  } catch (e) {
    // Client disconnected — ignore
  }
}

// ─── Entry point when run as standalone daemon process ────────────────────────

if (require.main === module || process.env.DEBUG_DAEMON_MODE === '1') {
  const config     = JSON.parse(process.env.DEBUG_DAEMON_CONFIG            || '{}');
  const sessionId  = process.env.DEBUG_DAEMON_SESSION_ID                   || '';
  const socketPath = process.env.DEBUG_DAEMON_SOCK_PATH                    || '';
  const snapshot   = process.env.DEBUG_DAEMON_SESSION_SNAPSHOT
    ? JSON.parse(process.env.DEBUG_DAEMON_SESSION_SNAPSHOT)
    : null;

  if (!config.host || !sessionId || !socketPath) {
    process.stderr.write('[debug-daemon] missing required env vars\n');
    process.exit(1);
  }

  startDaemon(config, sessionId, socketPath, snapshot).catch((err) => {
    process.stderr.write(`[debug-daemon] startup error: ${err.message}\n`);
    process.exit(1);
  });
}

module.exports = { startDaemon, _handleLine };
