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
const { stateFile, readState, writeState } = require('./connection-state');

function _stateFile(config) { return stateFile(config, 'abapgit-debug-active'); }
function _bpStateFile(config) { return stateFile(config, 'abapgit-bpstate'); }

/**
 * Return the deterministic Unix socket path for the debug daemon.
 * Computed from config so both cmdAttach and cmdStep can derive it independently.
 * @param {object} config - ABAP connection config
 * @returns {string}
 */
function getDaemonSocketPath(config) {
  return stateFile(config, 'abapgit-debug-daemon', '.sock');
}

/**
 * Persist the current list of breakpoints to a local tmp file.
 * @param {object} config - ABAP connection config
 * @param {Array}  bps    - Array of { id, object, uri, line, enabled }
 */
function saveBreakpointState(config, bps) {
  try {
    writeState(_bpStateFile(config), { breakpoints: bps, savedAt: Date.now() }, config);
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
    const data = readState(file, config);
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
    writeState(_stateFile(config), { ...state, savedAt: Date.now() }, config);
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
    return readState(file, config);
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
    writeState(file, { sessionId: null, clearedAt: Date.now() }, config);
  } catch (e) {
    // Ignore
  }
}

module.exports = { saveActiveSession, loadActiveSession, clearActiveSession, saveBreakpointState, loadBreakpointState, getDaemonSocketPath };
