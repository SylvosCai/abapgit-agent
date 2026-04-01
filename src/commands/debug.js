'use strict';

/**
 * Debug command — Interactive ABAP debugger via ADT REST API
 *
 * Sub-commands:
 *   set      — Set a breakpoint
 *   list     — List breakpoints
 *   delete   — Delete breakpoint(s)
 *   attach   — Attach to a debug session (blocks until breakpoint hit)
 *   step     — Step (over/into/out/continue) — AI mode
 *   vars     — Show variables — AI mode
 *   stack    — Show call stack — AI mode
 *   terminate — Terminate active session — AI mode
 *
 * Breakpoint management uses the POST synchronize model (verified against abap-adt-api):
 *   - POST /sap/bc/adt/debugger/breakpoints with the full desired list each time.
 *   - XML uses dbg: namespace, scope/terminalId/ideId attrs, syncScope element, #start=<line>.
 *   - Local state persisted in tmp so stateless CLI calls share the breakpoint list.
 *
 * Session management for AI/scripting mode (--json):
 *   attach --json spawns a background daemon (debug-daemon.js) that holds the
 *   stateful ADT HTTP connection open. step/vars/stack/terminate are thin IPC
 *   clients that connect to the daemon's Unix socket and exchange one JSON command
 *   per invocation. The daemon exits when terminate is called or after 30 min idle.
 *
 *   Human REPL mode (attach without --json) does NOT use the daemon — the process
 *   stays alive for the full session, so the HTTP connection is maintained naturally.
 */

const net  = require('net');
const path = require('path');
const { spawn } = require('child_process');
const { AdtHttp }      = require('../utils/adt-http');
const { DebugSession } = require('../utils/debug-session');
const debugStateModule = require('../utils/debug-state');
const { printHttpError } = require('../utils/format-error');
const {
  saveActiveSession,
  loadActiveSession,
  clearActiveSession
} = debugStateModule;

// Breakpoint state helpers — may be absent in unit-test mocks
const _saveBpState = debugStateModule.saveBreakpointState;
const _loadBpState = debugStateModule.loadBreakpointState;
// Daemon socket path — may be absent in unit-test mocks
const _getDaemonSocketPath = debugStateModule.getDaemonSocketPath;

const ADT_CLIENT_ID = 'ABAPGIT-AGENT-CLI';

// ─── Helpers ─────────────────────────────────────────────────────────────────

function val(args, flag) {
  const i = args.indexOf(flag);
  return i !== -1 && i + 1 < args.length ? args[i + 1] : null;
}

function hasFlag(args, flag) {
  return args.includes(flag);
}

/**
 * Valid class include types for --include flag.
 * User-facing names mirror the abapGit file suffixes (.clas.<name>.abap).
 * Maps user-facing name → ADT /includes/<type> path segment.
 * Verified by live ADT testing: breakpoints accepted for all three.
 *   testclasses → testclasses   → CCAU  (unit test class file)
 *   locals_imp  → implementations → CCIMP (local class implementations)
 *   locals_def  → definitions   → CCDEF (local class definitions)
 */
const CLASS_INCLUDE_TYPES = new Set(['testclasses', 'locals_imp', 'locals_def']);

const INCLUDE_TYPE_TO_ADT = {
  testclasses: 'testclasses',
  locals_imp:  'implementations',
  locals_def:  'definitions',
};

/**
 * Determine ADT object URI from object name (class/interface vs program vs include).
 * Must use /source/main suffix for classes — verified by live testing: ADT
 * rejects breakpoints set on the class root URI but accepts /source/main.
 *   classes  → /sap/bc/adt/oo/classes/<name_lowercase>/source/main
 *   includes → /sap/bc/adt/programs/includes/<name_lowercase>
 *   programs → /sap/bc/adt/programs/programs/<name_lowercase>
 *
 * Class method includes are named <ClassName padded to 30 chars with '='>CM<suffix>
 * e.g. ZCL_ABGAGT_AGENT=============CM00D
 * These must be routed to the programs/includes ADT endpoint.
 *
 * When includeType is supplied (testclasses|locals_imp|locals_def),
 * the URI targets the sub-include of the class instead of /source/main.
 * Line numbers are then section-local (from the .clas.<file>.abap file).
 */
function objectUri(name, includeType) {
  const upper = (name || '').toUpperCase();
  const lower = upper.toLowerCase();
  if (/^[ZY](CL|IF)_/.test(upper) || /^(ZCL|ZIF|YCL|YIF)/.test(upper)) {
    if (includeType && CLASS_INCLUDE_TYPES.has(includeType)) {
      const adtType = INCLUDE_TYPE_TO_ADT[includeType];
      return `/sap/bc/adt/oo/classes/${lower}/includes/${adtType}`;
    }
    return `/sap/bc/adt/oo/classes/${lower}/source/main`;
  }
  return `/sap/bc/adt/programs/programs/${lower}`;
}

/**
 * Build the breakpoints XML body for POST /sap/bc/adt/debugger/breakpoints.
 *
 * Format verified against abap-adt-api (marcellourbani/abap-adt-api):
 *   - Root element uses dbg: namespace (http://www.sap.com/adt/debugger)
 *   - Root attributes: scope, debuggingMode, requestUser, terminalId, ideId
 *   - Child <syncScope mode="full"> — required for the ADT handler to process
 *   - Each <breakpoint> uses adtcore:uri with #start=<line> fragment
 *   - kind="line" (not "breakpoint")
 *
 * @param {string} user - SAP logon user (requestUser)
 * @param {Array}  bps  - Array of { uri, line }
 * @param {string} [syncMode='full'] - 'full' replaces all; 'partial' merges
 */
function buildBreakpointsXml(user, bps, syncMode = 'full') {
  const inner = bps.map(bp => {
    const uriWithLine = `${bp.uri}#start=${bp.line}`;
    return `  <breakpoint xmlns:adtcore="http://www.sap.com/adt/core"` +
           ` kind="line" clientId="${ADT_CLIENT_ID}" skipCount="0"` +
           ` adtcore:uri="${uriWithLine}"/>`;
  }).join('\n');

  const requestUser = (user || '').toUpperCase();
  return `<?xml version="1.0" encoding="UTF-8"?>\n` +
    `<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger"\n` +
    `                 scope="external"\n` +
    `                 debuggingMode="user"\n` +
    `                 requestUser="${requestUser}"\n` +
    `                 terminalId="${ADT_CLIENT_ID}"\n` +
    `                 ideId="${ADT_CLIENT_ID}"\n` +
    `                 systemDebugging="false"\n` +
    `                 deactivated="false">\n` +
    `  <syncScope mode="${syncMode}"></syncScope>\n` +
    (inner ? inner + '\n' : '') +
    `</dbg:breakpoints>`;
}

/**
 * Parse breakpoints from AtomPub feed XML.
 * @param {string} xml
 * @returns {Array<{ id: string, object: string, line: string }>}
 */
function parseBreakpoints(xml) {
  const bps = [];
  // Match <entry> blocks
  const entryRe = /<entry>([\s\S]*?)<\/entry>/gi;
  let m;
  while ((m = entryRe.exec(xml)) !== null) {
    const inner = m[1];
    const id  = AdtHttp.extractXmlAttr(inner, 'id', null) || AdtHttp.extractXmlAttr(inner, 'adtdbg:id', null) || '';
    // Try to get URI from adtcore:objectReference
    const uriM = inner.match(/adtcore:uri="([^"]*)"/);
    const uri  = uriM ? uriM[1] : '';
    const line = AdtHttp.extractXmlAttr(inner, 'adtdbg:line', null) || AdtHttp.extractXmlAttr(inner, 'line', null) || '';
    // Extract object name from URI (last path segment), uppercase for display
    const object = uri ? uri.split('/').pop().toUpperCase() : '';
    if (id || uri) bps.push({ id, object, line, uri });
  }
  return bps;
}

/**
 * Parse server-assigned breakpoint IDs from the POST /breakpoints response body.
 * ADT returns:
 *   <dbg:breakpoints>
 *     <breakpoint id="KIND=0.SOURCETYPE=ABAP...." adtcore:uri="...#start=30" .../>
 *     <breakpoint errorMessage="Cannot create a breakpoint at this position" .../>
 *   </dbg:breakpoints>
 *
 * Returns an array parallel to the posted `bps` list with the server-assigned id
 * (or null if ADT rejected that breakpoint).
 *
 * @param {string} xml  - POST response body
 * @param {Array}  bps  - The breakpoints that were posted (in order)
 * @returns {Array<{ id: string|null, uri: string, line: number, error: string|null }>}
 */
function parseBreakpointResponse(xml, bps) {
  const results = [];
  const bpRe = /<breakpoint([^>]*)\/>/gi;
  let m;
  while ((m = bpRe.exec(xml)) !== null) {
    const attrs   = m[1];
    const id      = (attrs.match(/\bid="([^"]*)"/)            || [])[1] || null;
    const uriAttr = (attrs.match(/adtcore:uri="([^"]*)"/)     || [])[1] || '';
    const errMsg  = (attrs.match(/errorMessage="([^"]*)"/)    || [])[1] || null;
    // Extract line from #start=N fragment
    const lineM   = uriAttr.match(/#start=(\d+)/);
    const line    = lineM ? parseInt(lineM[1], 10) : null;
    const uri     = uriAttr.split('#')[0];
    results.push({ id: errMsg ? null : id, uri, line, error: errMsg || null });
  }
  return results;
}

/**
 * Re-POST the full breakpoint list to refresh them on the server.
 * Returns the updated bp list with server-assigned IDs and filters out
 * any that the server rejected (invalid position).
 *
 * @param {object} config
 * @param {AdtHttp} adt
 * @param {Array} bps  - current local bp list
 * @returns {Promise<{ valid: Array, stale: Array }>}
 */
async function refreshBreakpoints(config, adt, bps) {
  if (!bps || bps.length === 0) return { valid: [], stale: [] };

  let resp;
  try {
    const body = buildBreakpointsXml(config.user, bps);
    resp = await adt.post('/sap/bc/adt/debugger/breakpoints', body, {
      contentType: 'application/xml',
      headers: { Accept: 'application/xml' }
    });
  } catch (err) {
    // Non-fatal — return original list unchanged
    return { valid: bps, stale: [] };
  }

  const serverResults = parseBreakpointResponse(resp.body || '', bps);

  // Match server results back to local bps by uri+line
  const valid = [];
  const stale = [];
  for (const bp of bps) {
    const match = serverResults.find(r => r.uri === bp.uri && r.line === bp.line);
    if (match && match.error) {
      stale.push({ ...bp, error: match.error });
    } else if (match && match.id) {
      valid.push({ ...bp, id: match.id });
    } else {
      // No match in response — server silently dropped it (e.g. expired)
      stale.push({ ...bp, error: 'Not registered on server' });
    }
  }

  return { valid, stale };
}



/**
 * Parse a "name:line" token into { name, line }.
 * Accepts:
 *   - "ZCL_MY_CLASS:42"                    (--objects)
 *   - "src/zcl_my_class.clas.abap:42"      (--files)
 *
 * For file paths the object name is derived from the basename the same way
 * syntax.js does: first dot-segment uppercased.
 * Returns null if the token cannot be parsed.
 */
function parseBreakpointToken(token) {
  const lastColon = token.lastIndexOf(':');
  if (lastColon === -1) return null;

  const raw  = token.slice(0, lastColon);
  const lineN = parseInt(token.slice(lastColon + 1), 10);
  if (isNaN(lineN) || lineN < 1) return null;

  // Derive object name: if it looks like a file path use the basename
  const base = path.basename(raw);
  const name = base.includes('.') ? base.split('.')[0].toUpperCase() : raw.toUpperCase();
  return { name, line: lineN };
}

async function cmdSet(args, config, adt) {
  const objectName  = val(args, '--object');
  const lineRaw     = val(args, '--line');
  const filesArg    = val(args, '--files');
  const objectsArg  = val(args, '--objects');
  const includeType = val(args, '--include');
  const jsonOutput  = hasFlag(args, '--json');

  // Validate --include if supplied
  if (includeType && !CLASS_INCLUDE_TYPES.has(includeType)) {
    console.error(`  Error: --include must be one of: ${[...CLASS_INCLUDE_TYPES].join(', ')}`);
    process.exit(1);
  }

  // Collect all breakpoints to add from every accepted input form
  const toAdd = []; // [{ name, line }]

  // --files src/zcl_foo.clas.abap:42,src/zcl_bar.clas.abap:10
  if (filesArg) {
    for (const token of filesArg.split(',').map(s => s.trim()).filter(Boolean)) {
      const parsed = parseBreakpointToken(token);
      if (!parsed) {
        console.error(`  Error: --files value "${token}" must include a line number (e.g. src/zcl_foo.clas.abap:42)`);
        process.exit(1);
      }
      toAdd.push(parsed);
    }
  }

  // --objects ZCL_FOO:42,ZCL_BAR:10
  if (objectsArg) {
    for (const token of objectsArg.split(',').map(s => s.trim()).filter(Boolean)) {
      const parsed = parseBreakpointToken(token);
      if (!parsed) {
        console.error(`  Error: --objects value "${token}" must include a line number (e.g. ZCL_MY_CLASS:42)`);
        process.exit(1);
      }
      toAdd.push(parsed);
    }
  }

  // --object ZCL_FOO --line 42  (legacy / single-value form)
  if (objectName || lineRaw) {
    if (!objectName) {
      console.error('  Error: --object is required (e.g. --object ZCL_MY_CLASS)');
      process.exit(1);
    }
    if (!lineRaw) {
      console.error('  Error: --line is required (e.g. --line 42)');
      process.exit(1);
    }
    const line = parseInt(lineRaw, 10);
    if (isNaN(line) || line < 1) {
      console.error('  Error: --line must be a positive integer');
      process.exit(1);
    }
    toAdd.push({ name: objectName, line });
  }

  if (toAdd.length === 0) {
    console.error('  Error: specify breakpoint(s) via --files, --objects, or --object/--line');
    console.error('  Examples:');
    console.error('    debug set --files src/zcl_my_class.clas.abap:42');
    console.error('    debug set --objects ZCL_MY_CLASS:42');
    console.error('    debug set --object ZCL_MY_CLASS --line 42');
    console.error('    debug set --objects ZCL_MY_CLASS:16 --include testclasses');
    process.exit(1);
  }

  const existing = _loadBpState ? _loadBpState(config) : [];
  let updated = [...existing];
  const added = [];

  for (const { name, line } of toAdd) {
    const uri = objectUri(name, includeType);
    const objUpper = name.toUpperCase();
    // Skip if an identical breakpoint already exists
    if (existing.some(bp => bp.object === objUpper && bp.line === line)) {
      if (!jsonOutput) console.log(`\n  Already set: ${objUpper}:${line}`);
      continue;
    }
    const newId = `bp-${Date.now()}-${Math.random().toString(36).slice(2, 6)}`;
    updated.push({ id: newId, object: objUpper, uri, line });
    added.push({ name: objUpper, uri, line });
  }

  if (added.length === 0) {
    // Nothing new to register — all positions were already set
    if (jsonOutput) console.log(JSON.stringify([]));
    else console.log('');
    return;
  }

  let resp;
  try {
    await adt.fetchCsrfToken();
    const body = buildBreakpointsXml(config.user, updated);
    resp = await adt.post('/sap/bc/adt/debugger/breakpoints', body, {
      contentType: 'application/xml',
      headers: { Accept: 'application/xml' }
    });
  } catch (err) {
    printHttpError(err, { prefix: '  Error' });
    process.exit(1);
  }

  const serverResults = parseBreakpointResponse(resp ? resp.body || '' : '', updated);

  // Check for errors on newly added breakpoints
  for (const a of added) {
    const match = serverResults.find(r => r.uri === a.uri && r.line === a.line);
    if (match && match.error) {
      console.error(`\n  Error: ${match.error}\n`);
      process.exit(1);
    }
  }

  // Update local state with server-assigned IDs
  const updatedWithServerIds = updated.map(bp => {
    const sr = serverResults.find(r => r.uri === bp.uri && r.line === bp.line);
    return sr && sr.id ? { ...bp, id: sr.id } : bp;
  });
  if (_saveBpState) _saveBpState(config, updatedWithServerIds);

  // Immediately re-validate the newly added breakpoints: ADT accepts the POST
  // with HTTP 200 even for invalid positions (e.g. comment lines), but then
  // silently drops them.  Re-validating here gives immediate feedback instead
  // of only discovering the failure on the next "debug list".
  const addedBps = updatedWithServerIds.filter(bp =>
    added.some(a => a.uri === bp.uri && a.line === bp.line)
  );
  const { stale: newlyStale } = await refreshBreakpoints(config, adt, addedBps);
  if (newlyStale.length > 0) {
    // Remove stale from state
    const stillValid = updatedWithServerIds.filter(bp =>
      !newlyStale.some(s => s.uri === bp.uri && s.line === bp.line)
    );
    if (_saveBpState) _saveBpState(config, stillValid);

    if (jsonOutput) {
      console.log(JSON.stringify({ error: 'Breakpoint not accepted by server', stale: newlyStale.map(b => ({ object: b.object, line: b.line, error: b.error })) }));
    } else {
      newlyStale.forEach(({ object, line, error }) => {
        console.error(`\n  ❌ Breakpoint not accepted: ${object} line ${line}`);
        console.error(`     Reason: ${error || 'Not registered on server'}`);
        console.error(`     Tip: line must be an executable statement — not a comment, blank line,`);
        console.error(`          DATA declaration, or continuation line of a multi-line call.\n`);
      });
      process.exit(1);
    }
    return;
  }

  if (jsonOutput) {
    const out = added.map(a => {
      const sr = serverResults.find(r => r.uri === a.uri && r.line === a.line);
      return { id: (sr && sr.id) || null, object: a.name, line: a.line };
    });
    console.log(JSON.stringify(out.length === 1 ? out[0] : out));
    return;
  }

  for (const a of added) {
    console.log(`\n  Breakpoint set at ${a.name}:${a.line}`);
  }
  console.log('');
}

// ─── Sub-command: list ────────────────────────────────────────────────────────

async function cmdList(args, config, adt) {
  const jsonOutput = hasFlag(args, '--json');

  const localBps = _loadBpState ? _loadBpState(config) : [];

  if (localBps.length === 0) {
    if (jsonOutput) {
      console.log(JSON.stringify({ breakpoints: [] }));
    } else {
      console.log('\n  No breakpoints set.\n');
    }
    return;
  }

  // Always verify local breakpoints against the server.
  // Re-POSTing the full list refreshes them; the response body reveals which
  // ones ADT accepted vs rejected (invalid/expired position).
  await adt.fetchCsrfToken();
  const { valid, stale } = await refreshBreakpoints(config, adt, localBps);

  // Persist the refreshed list (with server IDs, stale ones removed)
  if (_saveBpState) _saveBpState(config, valid);

  const bps = valid;

  if (jsonOutput) {
    console.log(JSON.stringify({ breakpoints: bps, stale: stale.map(b => ({ object: b.object, line: b.line, error: b.error })) }));
    return;
  }

  if (stale.length > 0) {
    console.log(`\n  Warning: ${stale.length} breakpoint(s) were no longer valid on the server and have been removed:`);
    stale.forEach(({ object, line, error }) => {
      console.log(`    ${(object || '').padEnd(30)} line ${line}  (${error})`);
    });
  }

  if (bps.length === 0) {
    console.log('\n  No breakpoints set.\n');
    return;
  }

  console.log(`\n  Breakpoints (${bps.length})\n`);
  console.log('  ' + '#'.padEnd(4) + 'Object'.padEnd(35) + 'Line');
  console.log('  ' + '-'.repeat(50));
  bps.forEach(({ object, line }, i) => {
    console.log('  ' + String(i + 1).padEnd(4) + (object || '').padEnd(35) + (line || ''));
  });
  console.log('  ' + '-'.repeat(50));
  console.log('  Use "debug delete --object <name> --line <n>" or "debug delete --all"\n');
}

// ─── Sub-command: delete ──────────────────────────────────────────────────────

async function cmdDelete(args, config, adt) {
  const bpId       = val(args, '--id');
  const delObject  = val(args, '--object');
  const delLine    = val(args, '--line') ? parseInt(val(args, '--line'), 10) : null;
  const deleteAll  = hasFlag(args, '--all');
  const jsonOutput = hasFlag(args, '--json');

  if (!bpId && !deleteAll && !(delObject && delLine)) {
    console.error('  Error: Provide --object <name> --line <n>, --id <id>, or --all');
    process.exit(1);
  }

  const existing = _loadBpState ? _loadBpState(config) : [];

  let remaining;
  if (deleteAll) {
    remaining = [];
  } else if (delObject && delLine) {
    // Match by object name + line (case-insensitive)
    const upperObj = delObject.toUpperCase();
    remaining = existing.filter(bp => !(bp.object === upperObj && bp.line === delLine));
    if (remaining.length === existing.length) {
      console.error(`\n  Error: No breakpoint found at ${upperObj}:${delLine}\n`);
      process.exit(1);
    }
  } else {
    remaining = existing.filter(bp => bp.id !== bpId);
    if (remaining.length === existing.length) {
      // ID not found in local state (or no local state) — DELETE directly on server
      await adt.fetchCsrfToken();
      try {
        await adt.delete(`/sap/bc/adt/debugger/breakpoints/${encodeURIComponent(bpId)}`);
      } catch (err) {
        printHttpError(err, { prefix: '  Error' });
        process.exit(1);
      }
      if (jsonOutput) {
        console.log(JSON.stringify({ deleted: bpId }));
      } else {
        console.log(`\n  Breakpoint ${bpId} deleted.\n`);
      }
      return;
    }
  }

  // POST the remaining list (synchronize model — absent = deleted)
  await adt.fetchCsrfToken();
  const body = buildBreakpointsXml(config.user, remaining);

  try {
    await adt.post('/sap/bc/adt/debugger/breakpoints', body, {
      contentType: 'application/xml',
      headers: { Accept: 'application/xml' }
    });
  } catch (err) {
    // For delete-all, a DELETE to the collection endpoint is also valid
    if (deleteAll) {
      try {
        await adt.delete(`/sap/bc/adt/debugger/breakpoints?clientId=${encodeURIComponent(ADT_CLIENT_ID)}`);
      } catch (err2) {
        printHttpError(err2, { prefix: '  Error' });
        process.exit(1);
      }
    } else {
      printHttpError(err, { prefix: '  Error' });
      process.exit(1);
    }
  }

  if (_saveBpState) _saveBpState(config, remaining);

  const deletedLabel = deleteAll ? 'all'
    : (delObject && delLine) ? `${delObject.toUpperCase()}:${delLine}`
    : bpId;

  if (deleteAll) {
    if (jsonOutput) {
      console.log(JSON.stringify({ deleted: 'all' }));
    } else {
      console.log('\n  All breakpoints deleted.\n');
    }
  } else {
    if (jsonOutput) {
      console.log(JSON.stringify({ deleted: deletedLabel }));
    } else {
      console.log(`\n  Breakpoint ${deletedLabel} deleted.\n`);
    }
  }
}

// ─── Sub-command: attach ──────────────────────────────────────────────────────

async function cmdAttach(args, config, adt) {
  const sessionIdOverride = val(args, '--session');
  const jsonOutput = hasFlag(args, '--json');
  // Per-poll timeout in seconds sent to ADT (ADT blocks the POST for this long)
  const pollTimeout = parseInt(val(args, '--timeout') || '30', 10);
  // Shorter timeout used in takeover mode — keeps the connection alive but
  // lets ADT process stepContinue quickly and lets session 2 exit within
  // ~5 seconds of clearActiveSession being written.
  const takeoverPollTimeout = 5;
  // terminalId/ideId must match the breakpoint's ideId (ADT_CLIENT_ID) so that
  // ADT routes breakpoint hit notifications to this listener.
  const listenTerminalId = ADT_CLIENT_ID;

  if (!jsonOutput) {
    process.stderr.write('\n  Waiting for breakpoint hit... (run your ABAP program in a separate window)\n');
  }

  await adt.fetchCsrfToken();

  // Re-POST local breakpoints to refresh them on the server before listening.
  // Breakpoints expire when the SAP session or work process is restarted.
  // This ensures they are active regardless of when they were originally set.
  if (!sessionIdOverride) {
    const localBps = _loadBpState ? _loadBpState(config) : [];
    if (localBps.length === 0) {
      console.error('\n  No breakpoints set. Use "debug set --object <name> --line <n>" first.\n');
      process.exit(1);
    }
    const { valid, stale } = await refreshBreakpoints(config, adt, localBps);
    if (_saveBpState) _saveBpState(config, valid);
    if (stale.length > 0 && !jsonOutput) {
      process.stderr.write(`  Warning: ${stale.length} breakpoint(s) could not be registered (invalid position):\n`);
      stale.forEach(({ object, line, error }) => {
        process.stderr.write(`    ${(object || '').padEnd(30)} line ${line}  (${error})\n`);
      });
    }
    if (valid.length === 0) {
      console.error('\n  Error: All breakpoints were rejected by the server. Check line numbers point to executable statements.\n');
      process.exit(1);
    }
  }
  let sessionId = sessionIdOverride;
  let positionResult = null;

  if (!sessionId) {
    // ADT listeners: POST long-polls until a breakpoint is hit (or timeout).
    // On breakpoint hit the response body contains <DEBUGGEE_ID>; on timeout
    // (no breakpoint) returns 200 with empty body — poll again.
    const listenUrlBase = `/sap/bc/adt/debugger/listeners` +
      `?debuggingMode=user` +
      `&requestUser=${encodeURIComponent((config.user || '').toUpperCase())}` +
      `&terminalId=${encodeURIComponent(listenTerminalId)}` +
      `&ideId=${encodeURIComponent(listenTerminalId)}`;

    const MAX_POLLS = Math.ceil(240 / pollTimeout); // up to 4 minutes total
    // In takeover mode we switch to takeoverPollTimeout — recalculate the limit then.
    const MAX_TAKEOVER_POLLS = Math.ceil(240 / takeoverPollTimeout);
    let dots = 0;
    const attachStartedAt = Date.now(); // used to detect if another session takes over
    let takenOver = false;
    let firstPoll = true;

    for (let i = 0; i < (takenOver ? MAX_TAKEOVER_POLLS : MAX_POLLS); i++) {
      if (firstPoll) {
        process.stderr.write('  Listener active — trigger your ABAP program now.\n');
        firstPoll = false;
      }
      // Check whether another attach has won the race and saved a session since
      // this process started waiting.
      if (!takenOver) {
        const existingSession = loadActiveSession(config);
        if (existingSession && existingSession.savedAt && existingSession.savedAt > attachStartedAt) {
          const pos = existingSession.position;
          const where = pos && pos.line ? ` (${pos.class || pos.program || ''}:${pos.line})` : '';
          if (!jsonOutput) {
            process.stderr.write(`\n\n  Another session attached${where}. Waiting for it to finish...\n`);
          }
          takenOver = true;
          i = 0; // reset counter — takeover uses MAX_TAKEOVER_POLLS with shorter timeout
        }
      }

      let resp;
      try {
        const t = takenOver ? takeoverPollTimeout : pollTimeout;
        resp = await adt.post(`${listenUrlBase}&timeout=${t}`, '', {
          contentType: 'application/xml'
        });
      } catch (err) {
        if (err && err.statusCode === 406) {
          // Another listener (e.g. Eclipse) is active. Wait briefly and retry.
          await new Promise(r => setTimeout(r, 2000));
          continue;
        }
        if (err && err.statusCode === 404) {
          console.error(
            '\n  Debug session commands require ADT Debugger service (listeners).' +
            '\n  Check SICF node /sap/bc/adt/debugger/listeners is active.' +
            '\n  (The breakpoint management commands set/list/delete still work.)\n'
          );
          process.exit(1);
        }
        printHttpError(err, { prefix: '  Error from ADT listener' });
        process.exit(1);
      }

      if (resp.body && resp.body.trim().length > 0) {
        // Breakpoint hit — listener response contains <DEBUGGEE_ID> tag.
        // We extract the debuggeeId here; the actual attach (which returns
        // the debugSessionId) is done after the loop.
        const debuggeeIdMatch = resp.body.match(/<DEBUGGEE_ID>([^<]+)<\/DEBUGGEE_ID>/i) ||
                                resp.body.match(/DEBUGGEE_ID="([^"]+)"/i);
        sessionId = debuggeeIdMatch ? debuggeeIdMatch[1].trim() : null;

        if (!sessionId) {
          // Fallback: some ADT versions put session info in different attributes
          sessionId = AdtHttp.extractXmlAttr(resp.body, 'adtdbg:id', null) ||
                      AdtHttp.extractXmlAttr(resp.body, 'id', null) ||
                      (resp.headers && resp.headers['location']
                        ? resp.headers['location'].split('/').pop() : null);
        }

        if (!sessionId) {
          console.error('\n  Error: Breakpoint hit but could not parse DEBUGGEE_ID.\n');
          console.error('Response body:', resp.body.substring(0, 500));
          process.exit(1);
        }

        // Store the raw debuggee ID — it will be used in attach() below
        sessionId = sessionId; // debuggeeId, not yet the debugSessionId

        // In takeover mode a new breakpoint hit means the program was handed off
        // to us (e.g. after the other session's q). Don't steal the session —
        // just keep polling so the connection stays alive. But check for session
        // end first (same post-poll check as the empty-body path).
        if (takenOver) {
          sessionId = null;
          const currentSession = loadActiveSession(config);
          if (!currentSession || !currentSession.sessionId) {
            if (!jsonOutput) process.stderr.write('  Session ended.\n\n');
            process.exit(0);
          }
          continue;
        }
        break;
      }

      // Empty body = timeout, no breakpoint hit yet — poll again
      // After a takeover, check here (post-poll) whether the other session ended.
      // Checking after the poll ensures ADT had a live listener during its
      // stepContinue processing window before we disconnect.
      if (takenOver) {
        const currentSession = loadActiveSession(config);
        if (!currentSession || !currentSession.sessionId) {
          if (!jsonOutput) process.stderr.write('  Session ended.\n\n');
          process.exit(0);
        }
      }
      if (!jsonOutput && !takenOver) {
        dots++;
        if (dots % 3 === 0) process.stderr.write('.');
      }
    }

    if (!sessionId) {
      if (takenOver) {
        // The other session was still active when we hit the poll limit.
        if (!jsonOutput) process.stderr.write('  Listener timed out waiting for other session to finish.\n\n');
        process.exit(0);
      }
      console.error('\n  Timeout: No breakpoint was hit within 4 minutes.\n');
      process.exit(1);
    }
  }

  const session = new DebugSession(adt, sessionId);

  // If we got here via the listener (not --session override), we have a
  // DEBUGGEE_ID and need to call ?method=attach to register as the active
  // debugger for that work process. Without this step, all subsequent calls
  // (getStack, getVariables, step) return "noSessionAttached" (T100-530).
  if (!sessionIdOverride) {
    if (!jsonOutput) {
      process.stderr.write('\n  Attaching to debug session...\n');
    }
    try {
      await session.attach(sessionId, (config.user || '').toUpperCase());
    } catch (e) {
      printHttpError(e, { prefix: '  Error during attach' });
      if (e.body) console.error('  Response body:', e.body.substring(0, 400));
      process.exit(1);
    }
  }

  // Fetch position + variables now that we have a live session
  try {
    positionResult = await session.getPosition();
  } catch (e) {
    // Position fetch is best-effort; proceed with empty state
    positionResult = { position: {}, source: [] };
  }

  // Dummy loop body retained for structural compatibility (never executes)
  if (false) {
    const MAX_POLLS = 0;
    const POLL_INTERVAL = 0;
    for (let i = 0; i < MAX_POLLS; i++) {
      try {
        const frames = await session.getStack();
        if (frames && frames.length > 0 && frames[0].line > 0) {
          positionResult = await session.getPosition();
          break;
        }
      } catch (e) {
        // Stack not ready yet — keep polling
      }
      await new Promise(r => setTimeout(r, POLL_INTERVAL));
    }
  }

  const { position, source } = positionResult;
  saveActiveSession(config, { sessionId, position });

  if (jsonOutput) {
    // Spawn the background daemon to hold the stateful ADT HTTP connection.
    // The daemon process inherits the exact SAP_SESSIONID cookie + CSRF token
    // via the snapshot env var, so it reuses the same ABAP work process.
    const socketPath = _getDaemonSocketPath ? _getDaemonSocketPath(config) : null;
    if (socketPath) {
      const snapshot = { csrfToken: adt.csrfToken, cookies: adt.cookies };
      const daemonEnv = {
        ...process.env,
        DEBUG_DAEMON_MODE:            '1',
        DEBUG_DAEMON_CONFIG:          JSON.stringify(config),
        DEBUG_DAEMON_SESSION_ID:      sessionId,
        DEBUG_DAEMON_SOCK_PATH:       socketPath,
        DEBUG_DAEMON_SESSION_SNAPSHOT: JSON.stringify(snapshot)
      };
      const daemonScript = path.resolve(__dirname, '../utils/debug-daemon.js');
      const child = spawn(process.execPath, [daemonScript], {
        detached: true,
        stdio: ['ignore', 'ignore', 'ignore'],
        env: daemonEnv
      });
      child.unref();

      // Wait for daemon socket to appear (up to 5 s) before returning JSON
      try {
        await waitForSocket(socketPath, 5000);
        // Persist socket path in session state so step/vars/stack/terminate find it
        saveActiveSession(config, { sessionId, position, socketPath });
      } catch (e) {
        // Non-fatal: fall back to stateless direct-ADT mode
      }
    }

    console.log(JSON.stringify({ session: sessionId, position, source }));
    return;
  }

  // Human mode — enter interactive REPL
  process.stderr.write('\n');
  const { startRepl } = require('../utils/debug-repl');
  await startRepl(session, positionResult, () => clearActiveSession(config));
  clearActiveSession(config); // fallback if onBeforeExit path isn't reached
}

// ─── Sub-command: step ────────────────────────────────────────────────────────

async function cmdStep(args, config, adt) {
  const typeMap = { over: 'stepOver', into: 'stepInto', out: 'stepReturn', continue: 'stepContinue' };
  const typeRaw = val(args, '--type') || 'over';
  const type = typeMap[typeRaw] || 'stepOver';
  const jsonOutput = hasFlag(args, '--json');
  const { sessionId, socketPath } = resolveSessionState(args, config);

  // Prefer daemon IPC when a socket path is known
  if (socketPath) {
    let resp;
    try {
      resp = await sendDaemonCommand(socketPath, { cmd: 'step', type }, 60000);
    } catch (err) {
      printHttpError(err, { prefix: '  Error' });
      process.exit(1);
    }
    if (!resp.ok) {
      console.error(`\n  Error: ${resp.error}${resp.statusCode ? ` (HTTP ${resp.statusCode})` : ''}${resp.body ? '\n  Body: ' + resp.body.substring(0, 400) : ''}\n`);
      process.exit(1);
    }
    // continued+finished (or empty position) means the program ran to completion
    const doneByFlag = resp.position && resp.position.finished;
    const doneByEmpty = !resp.position || (!resp.position.class && !resp.position.method && !resp.position.program);
    if (doneByFlag || doneByEmpty) {
      clearActiveSession(config);
      if (jsonOutput) {
        console.log(JSON.stringify({ position: resp.position || {}, source: [], finished: true }));
        return;
      }
      console.log('\n  Execution completed — no active breakpoint. Debug session ended.\n');
      return;
    }
    saveActiveSession(config, { sessionId, position: resp.position, socketPath });
    if (jsonOutput) {
      console.log(JSON.stringify({ position: resp.position, source: resp.source }));
      return;
    }
    const { renderState } = require('../utils/debug-repl');
    renderState(resp.position, resp.source, []);
    return;
  }

  // Fallback: direct ADT call (no daemon running — e.g. human REPL or test mode)
  if (!adt.csrfToken) await adt.fetchCsrfToken();
  const session = new DebugSession(adt, sessionId);

  let result;
  try {
    result = await session.step(type);
  } catch (err) {
    if (err && err.statusCode === 404) {
      console.error(
        '\n  Debug session commands require ADT Debugger service (listeners).' +
        '\n  Check SICF node /sap/bc/adt/debugger/listeners is active.\n'
      );
      process.exit(1);
    }
    printHttpError(err, { prefix: '  Error' });
    process.exit(1);
  }

  saveActiveSession(config, { sessionId, position: result.position });

  const doneByFlag  = result.position && result.position.finished;
  const doneByEmpty = !result.position || (!result.position.class && !result.position.method && !result.position.program);
  if (doneByFlag || doneByEmpty) {
    clearActiveSession(config);
    if (jsonOutput) {
      console.log(JSON.stringify({ position: result.position || {}, source: [], finished: true }));
      return;
    }
    console.log('\n  Execution completed — no active breakpoint. Debug session ended.\n');
    return;
  }

  if (jsonOutput) {
    console.log(JSON.stringify({ position: result.position, source: result.source }));
    return;
  }

  const { renderState } = require('../utils/debug-repl');
  renderState(result.position, result.source, []);
}

// ─── Sub-command: vars ────────────────────────────────────────────────────────

async function cmdVars(args, config, adt) {
  const nameFilter = val(args, '--name');
  const expandName = val(args, '--expand');
  const jsonOutput = hasFlag(args, '--json');
  const { sessionId, socketPath } = resolveSessionState(args, config);

  // --expand <name>: drill into a named complex variable (internal table / structure)
  if (expandName) {
    return cmdExpand(expandName, sessionId, socketPath, config, adt, jsonOutput);
  }

  // Prefer daemon IPC
  if (socketPath) {
    let resp;
    try {
      resp = await sendDaemonCommand(socketPath, { cmd: 'vars', name: nameFilter }, 60000);
    } catch (err) {
      printHttpError(err, { prefix: '  Error' });
      process.exit(1);
    }
    if (!resp.ok) {
      console.error(`\n  Error: ${resp.error}${resp.statusCode ? ` (HTTP ${resp.statusCode})` : ''}\n`);
      process.exit(1);
    }
    const variables = resp.variables;
    if (jsonOutput) {
      console.log(JSON.stringify({ variables }));
      return;
    }
    _printVars(variables);
    return;
  }

  // Fallback: direct ADT call
  if (!adt.csrfToken) await adt.fetchCsrfToken();
  const session = new DebugSession(adt, sessionId);

  let variables;
  try {
    variables = await session.getVariables(nameFilter);
  } catch (err) {
    if (err && err.statusCode === 404) {
      console.error(
        '\n  Debug session commands require ADT Debugger service (listeners).' +
        '\n  Check SICF node /sap/bc/adt/debugger/listeners is active.\n'
      );
      process.exit(1);
    }
    printHttpError(err, { prefix: '  Error' });
    process.exit(1);
  }

  if (jsonOutput) {
    console.log(JSON.stringify({ variables }));
    return;
  }

  _printVars(variables);
}

/**
 * Drill one level into a named complex variable.
 * Supports `->` path notation for nested expansion, e.g.:
 *   LO_FACTORY->MT_COMMAND_MAP   (object attr → table)
 *   LS_DATA->COMPONENT            (structure field)
 *
 * When a daemon socket is active, uses daemon IPC for single-segment paths.
 * Multi-segment paths always run directly against ADT (DebugSession.expandPath).
 */
async function cmdExpand(expandName, sessionId, socketPath, config, adt, jsonOutput) {
  // Split on -> to detect path notation. Also handle --> typos by stripping stray dashes.
  // Normalize ABAP-style field accessors to use -> separator:
  //   [N]-FIELD  → [N]->FIELD   (array row then struct field)
  //   *-FIELD    → *->FIELD     (dereference then struct field: lr_request->*-files)
  const normalizedName = expandName
    .replace(/\](-(?!>))/g,  ']->') // [N]-FIELD → [N]->FIELD
    .replace(/\*(-(?!>))/g,  '*->'); // *-FIELD   → *->FIELD
  const pathParts = normalizedName.split('->').map(s => s.replace(/^-+|-+$/g, '').trim()).filter(Boolean);

  // Multi-segment path: must go direct (daemon IPC only handles one level at a time)
  if (pathParts.length > 1) {
    if (!adt.csrfToken) await adt.fetchCsrfToken();
    const session = new DebugSession(adt, sessionId);
    let result;
    try {
      result = await session.expandPath(pathParts);
    } catch (err) {
      printHttpError(err, { prefix: '  Error' });
      process.exit(1);
    }
    const { variable: target, children } = result;
    return _printExpandResult(expandName, target, children, jsonOutput);
  }

  // Single-segment path — existing logic (daemon-aware)
  // Step 1: get all vars to find the ID of the target variable
  let allVars;
  if (socketPath) {
    let resp;
    try {
      resp = await sendDaemonCommand(socketPath, { cmd: 'vars', name: null }, 60000);
    } catch (err) {
      printHttpError(err, { prefix: '  Error' });
      process.exit(1);
    }
    if (!resp.ok) {
      console.error(`\n  Error: ${resp.error}\n`);
      process.exit(1);
    }
    allVars = resp.variables;
  } else {
    if (!adt.csrfToken) await adt.fetchCsrfToken();
    const session = new DebugSession(adt, sessionId);
    allVars = await session.getVariables(null).catch(() => []);
  }

  const target = allVars.find(v => v.name.toUpperCase() === expandName.toUpperCase());
  if (!target) {
    console.error(`\n  Error: Variable '${expandName}' not found. Run 'debug vars' to list variables.\n`);
    process.exit(1);
  }
  if (!target.id) {
    console.error(`\n  Error: Variable '${expandName}' has no ADT ID — cannot expand.\n`);
    process.exit(1);
  }

  // Step 2: expand (drill one level)
  const meta = { metaType: target.metaType || '', tableLines: target.tableLines || 0 };
  let children;
  if (socketPath) {
    let resp;
    try {
      resp = await sendDaemonCommand(socketPath, { cmd: 'expand', id: target.id, meta }, 60000);
    } catch (err) {
      printHttpError(err, { prefix: '  Error' });
      process.exit(1);
    }
    if (!resp.ok) {
      console.error(`\n  Error: ${resp.error}\n`);
      process.exit(1);
    }
    children = resp.variables;
  } else {
    const session = new DebugSession(adt, sessionId);
    children = await session.getVariableChildren(target.id, meta).catch(() => []);
  }

  return _printExpandResult(expandName, target, children, jsonOutput);
}

function _printExpandResult(label, variable, children, jsonOutput) {
  if (jsonOutput) {
    console.log(JSON.stringify({ variable, children }));
    return;
  }

  if (children.length === 0) {
    console.log(`\n  ${label} — no children (scalar or empty).\n`);
    return;
  }

  // Compute column widths from actual data (min 4/4, capped at 50/25).
  const nameW = Math.min(50, Math.max(4, ...children.map(c => (c.name || '').length)));
  const typeW = Math.min(25, Math.max(4, ...children.map(c => (c.type || c.metaType || '').length)));

  console.log(`\n  ${label} (${variable.type || variable.metaType || '?'}):\n`);
  console.log('  ' + 'Name'.padEnd(nameW + 2) + 'Type'.padEnd(typeW + 2) + 'Value');
  console.log('  ' + '-'.repeat(nameW + typeW + 24));
  children.forEach(({ name, type, value, metaType, tableLines }) => {
    const displayType  = (type || metaType || '').slice(0, typeW);
    const displayValue = metaType === 'table'
      ? `[${tableLines} rows] — use 'x ${label}->${name}' to expand`
      : value;
    console.log('  ' + name.padEnd(nameW + 2) + displayType.padEnd(typeW + 2) + displayValue);
  });
  console.log('');
}

function _printVars(variables) {
  const { printVarList } = require('../utils/debug-render');
  printVarList(variables);
}

// ─── Sub-command: stack ───────────────────────────────────────────────────────

async function cmdStack(args, config, adt) {
  const jsonOutput = hasFlag(args, '--json');
  const { sessionId, socketPath } = resolveSessionState(args, config);

  // Prefer daemon IPC
  if (socketPath) {
    let resp;
    try {
      resp = await sendDaemonCommand(socketPath, { cmd: 'stack' }, 60000);
    } catch (err) {
      printHttpError(err, { prefix: '  Error' });
      process.exit(1);
    }
    if (!resp.ok) {
      console.error(`\n  Error: ${resp.error}${resp.statusCode ? ` (HTTP ${resp.statusCode})` : ''}${resp.body ? '\n  Body: ' + resp.body.substring(0, 400) : ''}\n`);
      process.exit(1);
    }
    const frames = resp.frames;
    if (jsonOutput) {
      console.log(JSON.stringify({ frames }));
      return;
    }
    _printStack(frames);
    return;
  }

  // Fallback: direct ADT call
  if (!adt.csrfToken) await adt.fetchCsrfToken();
  const session = new DebugSession(adt, sessionId);

  let frames;
  try {
    frames = await session.getStack();
  } catch (err) {
    if (err && err.statusCode === 404) {
      console.error(
        '\n  Debug session commands require ADT Debugger service (listeners).' +
        '\n  Check SICF node /sap/bc/adt/debugger/listeners is active.\n'
      );
      process.exit(1);
    }
    printHttpError(err, { prefix: '  Error' });
    process.exit(1);
  }

  if (jsonOutput) {
    console.log(JSON.stringify({ frames }));
    return;
  }

  _printStack(frames);
}

function _printStack(frames) {
  if (frames.length === 0) {
    console.log('\n  No call stack available.\n');
    return;
  }
  console.log('\n  Call Stack:\n');
  frames.forEach(({ frame, class: cls, method, line }) => {
    const loc = cls ? `${cls}->${method}` : method;
    console.log(`  ${String(frame).padStart(3)}  ${loc}  (line ${line})`);
  });
  console.log('');
}

// ─── Sub-command: terminate ───────────────────────────────────────────────────

async function cmdTerminate(args, config, adt) {
  const jsonOutput = hasFlag(args, '--json');
  const { sessionId, socketPath } = resolveSessionState(args, config);

  // Prefer daemon IPC — daemon calls session.terminate() then exits itself
  if (socketPath) {
    let resp;
    try {
      resp = await sendDaemonCommand(socketPath, { cmd: 'terminate' }, 30000);
    } catch (err) {
      // Socket may be gone already (daemon crashed or timed out) — treat as terminated
      resp = { ok: true, terminated: true };
    }
    clearActiveSession(config);
    if (jsonOutput) {
      console.log(JSON.stringify({ terminated: resp.ok ? true : resp.error }));
      return;
    }
    console.log('\n  Debug session terminated.\n');
    return;
  }

  // Fallback: direct ADT call
  const session = new DebugSession(adt, sessionId);

  try {
    await adt.fetchCsrfToken();
    await session.terminate();
  } catch (err) {
    if (err && err.statusCode === 404) {
      console.error(
        '\n  Debug session commands require ADT Debugger service (listeners).' +
        '\n  Check SICF node /sap/bc/adt/debugger/listeners is active.\n'
      );
      process.exit(1);
    }
    printHttpError(err, { prefix: '  Error' });
    process.exit(1);
  }

  clearActiveSession(config);

  if (jsonOutput) {
    console.log(JSON.stringify({ terminated: true }));
    return;
  }

  console.log('\n  Debug session terminated.\n');
}

// ─── Daemon IPC helpers ───────────────────────────────────────────────────────

/**
 * Wait for the daemon's Unix socket to appear (after spawn).
 * @param {string} socketPath
 * @param {number} timeoutMs
 */
function waitForSocket(socketPath, timeoutMs) {
  return new Promise((resolve, reject) => {
    const deadline = Date.now() + timeoutMs;
    function check() {
      const client = net.createConnection(socketPath);
      client.on('connect', () => {
        client.destroy();
        resolve();
      });
      client.on('error', () => {
        if (Date.now() >= deadline) {
          reject(new Error('Timeout waiting for debug daemon to start'));
        } else {
          setTimeout(check, 100);
        }
      });
    }
    check();
  });
}

/**
 * Send one JSON command to the daemon and return the parsed JSON response.
 * @param {string} socketPath
 * @param {object} command
 * @param {number} timeoutMs
 * @returns {Promise<object>}
 */
function sendDaemonCommand(socketPath, command, timeoutMs) {
  return new Promise((resolve, reject) => {
    const client = net.createConnection(socketPath);
    let buf = '';
    let timer = null;

    function cleanup() {
      if (timer) { clearTimeout(timer); timer = null; }
      try { client.destroy(); } catch (e) { /* ignore */ }
    }

    timer = setTimeout(() => {
      cleanup();
      reject(new Error('Timeout waiting for daemon response'));
    }, timeoutMs);

    client.on('connect', () => {
      client.write(JSON.stringify(command) + '\n');
    });

    client.on('data', (chunk) => {
      buf += chunk.toString();
      const idx = buf.indexOf('\n');
      if (idx !== -1) {
        const line = buf.slice(0, idx).trim();
        cleanup();
        try {
          resolve(JSON.parse(line));
        } catch (e) {
          reject(new Error(`Invalid JSON from daemon: ${line}`));
        }
      }
    });

    client.on('error', (err) => {
      cleanup();
      reject(err);
    });

    client.on('close', () => {
      if (timer) {
        cleanup();
        reject(new Error('Daemon closed connection without responding'));
      }
    });
  });
}

// ─── Session ID resolution ────────────────────────────────────────────────────

/**
 * Resolve session state for AI-mode sub-commands.
 * Returns { sessionId, socketPath } where socketPath may be null if the
 * daemon is not running (falls back to direct ADT calls).
 */
function resolveSessionState(args, config) {
  if (hasFlag(args, '--session')) {
    console.error(
      '\n  Error: --session is not valid for this command.' +
      '\n  step/vars/stack/terminate communicate via the daemon socket, not a raw session ID.' +
      '\n  Just run the command without --session — the active session is loaded automatically.\n'
    );
    process.exit(1);
  }

  const state = loadActiveSession(config);
  if (state && state.sessionId) {
    return { sessionId: state.sessionId, socketPath: state.socketPath || null };
  }

  console.error('\n  Error: No active debug session. Run "debug attach" first.\n');
  process.exit(1);
}

/** @deprecated Use resolveSessionState */
function resolveSessionId(args, config) {
  return resolveSessionState(args, config).sessionId;
}

// ─── Usage ───────────────────────────────────────────────────────────────────

function printUsage() {
  console.log(`
  Usage: abapgit-agent debug <subcommand> [options]

  Breakpoint Management:
    set     --files <file>:<n>[,...]             Set breakpoint(s) from local source files
    set     --objects <name>:<n>[,...]           Set breakpoint(s) by object name (no local file needed)
    set     --object <name> --line <n>           Set a single breakpoint (legacy form)
    list                                         List all breakpoints
    delete  --object <name> --line <n>           Delete a specific breakpoint
    delete  --id <id>                               Delete by server ID (from --json output)
    delete  --all                                   Delete all breakpoints

  Debug Session (Human REPL mode):
    attach                                 Attach and enter interactive REPL

  Debug Session (AI / scripting mode):
    attach  --json                         Attach, wait for breakpoint, return JSON
    step    [--type over|into|out|continue] [--json]  Execute a step
    vars    [--name <var>] [--json]         Show variables
    vars    --expand <var> [--json]         Drill into a complex variable (table/structure)
    stack   [--json]                        Show call stack
    terminate [--json]                      Terminate session

  Examples:
    abapgit-agent debug set --files src/zcl_my_class.clas.abap:42
    abapgit-agent debug set --objects ZCL_MY_CLASS:42
    abapgit-agent debug set --object ZCL_MY_CLASS --line 42
    abapgit-agent debug list
    abapgit-agent debug attach
    abapgit-agent debug attach --json
    abapgit-agent debug step --type over --json
    abapgit-agent debug vars --json
    abapgit-agent debug stack --json
    abapgit-agent debug terminate --json
    abapgit-agent debug delete --all
`);
}

// ─── Module export ────────────────────────────────────────────────────────────

module.exports = {
  name: 'debug',
  description: 'Interactive ABAP debugger via ADT REST API',
  requiresAbapConfig: true,
  requiresVersionCheck: false,

  async execute(args, context) {
    const { loadConfig, AdtHttp: AdtHttpCtx } = context;
    const subcommand = args[0];
    const subArgs = args.slice(1);

    if (!subcommand || subcommand === '--help' || subcommand === '-h') {
      printUsage();
      return;
    }

    const config = loadConfig();
    // Allow test injection via context; otherwise use the real AdtHttp
    const HttpClass = (context && context.AdtHttpClass) || AdtHttp;
    const adt = new HttpClass(config);

    switch (subcommand) {
      case 'set':
        return cmdSet(subArgs, config, adt);

      case 'list':
        return cmdList(subArgs, config, adt);

      case 'delete':
        return cmdDelete(subArgs, config, adt);

      case 'attach':
        return cmdAttach(subArgs, config, adt);

      case 'step':
        return cmdStep(subArgs, config, adt);

      case 'vars':
        return cmdVars(subArgs, config, adt);

      case 'stack':
        return cmdStack(subArgs, config, adt);

      case 'terminate':
        return cmdTerminate(subArgs, config, adt);

      default:
        console.error(`  Unknown debug subcommand: ${subcommand}`);
        printUsage();
        process.exit(1);
    }
  }
};
