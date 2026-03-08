'use strict';

/**
 * Debug session state persistence for AI / scripting mode.
 * Saves active session ID to a tmp file so individual stateless sub-commands
 * (debug step, debug vars, debug stack, debug terminate) can share session state
 * without requiring --session on every call.
 *
 * Also provides breakpoint state persistence so that the full breakpoint list
 * can be maintained locally (required for PUT/synchronize model of ADT API).
 */
const fs = require('fs');
const path = require('path');
const os = require('os');
const crypto = require('crypto');

function _stateFile(config) {
  const hash = crypto.createHash('md5')
    .update(`${config.host}:${config.user}:${config.client}`)
    .digest('hex')
    .substring(0, 8);
  return path.join(os.tmpdir(), `abapgit-debug-active-${hash}.json`);
}

function _bpStateFile(config) {
  const hash = crypto.createHash('md5')
    .update(`${config.host}:${config.user}:${config.client}`)
    .digest('hex')
    .substring(0, 8);
  return path.join(os.tmpdir(), `abapgit-bpstate-${hash}.json`);
}

/**
 * Return the deterministic Unix socket path for the debug daemon.
 * Computed from config so both cmdAttach and cmdStep can derive it independently.
 * @param {object} config - ABAP connection config
 * @returns {string}
 */
function getDaemonSocketPath(config) {
  const hash = crypto.createHash('md5')
    .update(`${config.host}:${config.user}:${config.client}`)
    .digest('hex')
    .substring(0, 8);
  return path.join(os.tmpdir(), `abapgit-debug-daemon-${hash}.sock`);
}

/**
 * Persist the current list of breakpoints to a local tmp file.
 * @param {object} config - ABAP connection config
 * @param {Array}  bps    - Array of { id, object, uri, line, enabled }
 */
function saveBreakpointState(config, bps) {
  try {
    fs.writeFileSync(_bpStateFile(config), JSON.stringify({ breakpoints: bps, savedAt: Date.now() }), 'utf8');
  } catch (e) {
    // Non-fatal
  }
}

/**
 * Load the locally-persisted breakpoint list.
 * @param {object} config - ABAP connection config
 * @returns {Array} Array of breakpoint objects (may be empty)
 */
function loadBreakpointState(config) {
  const file = _bpStateFile(config);
  if (!fs.existsSync(file)) return [];
  try {
    const data = JSON.parse(fs.readFileSync(file, 'utf8'));
    return Array.isArray(data.breakpoints) ? data.breakpoints : [];
  } catch (e) {
    return [];
  }
}

/**
 * Persist the active debug session after a successful attach.
 * @param {object} config - ABAP connection config
 * @param {object} state  - { sessionId, position }
 */
function saveActiveSession(config, state) {
  try {
    fs.writeFileSync(_stateFile(config), JSON.stringify({ ...state, savedAt: Date.now() }), 'utf8');
  } catch (e) {
    // Non-fatal — AI mode will require --session explicitly if file can't be written
  }
}

/**
 * Load the active debug session saved by a previous attach.
 * @param {object} config - ABAP connection config
 * @returns {{ sessionId: string, position: object }|null} Session state or null
 */
function loadActiveSession(config) {
  const file = _stateFile(config);
  if (!fs.existsSync(file)) return null;
  try {
    return JSON.parse(fs.readFileSync(file, 'utf8'));
  } catch (e) {
    return null;
  }
}

/**
 * Mark the active session as ended.
 *
 * Writes { sessionId: null } rather than deleting the file so that any other
 * process polling in takeover mode can detect the cleared state immediately,
 * even when it is currently handling a non-empty (breakpoint-hit) ADT response.
 * Checking !currentSession.sessionId covers both "file deleted" (null object)
 * and "file present but sessionId is null".
 *
 * @param {object} config - ABAP connection config
 */
function clearActiveSession(config) {
  try {
    const file = _stateFile(config);
    fs.writeFileSync(file, JSON.stringify({ sessionId: null, clearedAt: Date.now() }), 'utf8');
  } catch (e) {
    // Ignore
  }
}

module.exports = { saveActiveSession, loadActiveSession, clearActiveSession, saveBreakpointState, loadBreakpointState, getDaemonSocketPath };
