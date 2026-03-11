'use strict';

/**
 * DebugSession — stateful wrapper around ADT debugger REST API
 *
 * All operations use POST /sap/bc/adt/debugger?method=<name>.
 * The session is bound to a specific ABAP work process via:
 *   X-sap-adt-sessiontype: stateful
 * Without this header each request may land on a different work process
 * that has no debug state, causing "noSessionAttached" (T100KEY-NO=530).
 *
 * API sequence:
 *   1. POST /sap/bc/adt/debugger/listeners  → DEBUGGEE_ID
 *   2. POST /sap/bc/adt/debugger?method=attach&debuggeeId=X  → debugSessionId
 *   3. POST /sap/bc/adt/debugger?method=getStack  → stack frames
 *   4. POST /sap/bc/adt/debugger?method=getChildVariables  → hierarchy (parent→child IDs)
 *   5. POST /sap/bc/adt/debugger?method=getVariables  → variable values for leaf IDs
 *   6. POST /sap/bc/adt/debugger?method=stepOver|stepInto|stepReturn|stepContinue
 *   7. POST /sap/bc/adt/debugger?method=terminateDebuggee
 */
const fs = require('fs');
const path = require('path');
const { AdtHttp } = require('./adt-http');

// Header required to pin all requests to the same ABAP work process.
const STATEFUL_HEADER = { 'X-sap-adt-sessiontype': 'stateful' };

/**
 * Retry a debug ADT call up to maxRetries times on transient ICM errors.
 *
 * The SAP ICM (load balancer) returns HTTP 400 with an HTML "Service cannot
 * be reached" body when the target ABAP work process is momentarily unavailable
 * (e.g. finishing a previous step, or briefly between requests).  This is a
 * transient condition that resolves within a second or two — retrying is safe
 * for all debug read/navigation operations.
 *
 * @param {function} fn         - Async function to retry (takes no args)
 * @param {number}   maxRetries - Max additional attempts after the first (default 3)
 * @param {number}   delayMs    - Wait between retries in ms (default 1000)
 */
async function retryOnIcmError(fn, maxRetries = 12, delayMs = 2000) {
  let lastErr;
  for (let attempt = 0; attempt <= maxRetries; attempt++) {
    try {
      return await fn();
    } catch (err) {
      const isIcmError = err && err.statusCode === 400 &&
        err.body && err.body.includes('Service cannot be reached');
      if (!isIcmError) throw err;
      lastErr = err;
      if (attempt < maxRetries) {
        await new Promise(r => setTimeout(r, delayMs));
      }
    }
  }
  throw lastErr;
}

class DebugSession {
  /**
   * @param {AdtHttp} adtHttp   - ADT HTTP client instance (carries session cookie)
   * @param {string}  sessionId - Active debug session ID (debugSessionId from attach)
   */
  constructor(adtHttp, sessionId) {
    this.http = adtHttp;
    this.sessionId = sessionId;
  }

  /**
   * Attach to a paused ABAP work process.
   * Must be called after the listener returns a DEBUGGEE_ID.
   *
   * @param {string} debuggeeId - Value of <DEBUGGEE_ID> from listener response
   * @param {string} requestUser - SAP logon user (uppercase)
   * @returns {Promise<string>} The debugSessionId to use for all subsequent calls
   */
  async attach(debuggeeId, requestUser) {
    const url = `/sap/bc/adt/debugger?method=attach` +
      `&debuggeeId=${encodeURIComponent(debuggeeId)}` +
      `&dynproDebugging=true` +
      `&debuggingMode=user` +
      `&requestUser=${encodeURIComponent(requestUser)}`;

    const { body } = await this.http.post(url, '', {
      contentType: 'application/vnd.sap.as+xml',
      headers: STATEFUL_HEADER
    });

    // Response: <dbg:attach debugSessionId="..." isSteppingPossible="true" ...>
    const debugSessionId =
      AdtHttp.extractXmlAttr(body, 'dbg:attach', 'debugSessionId') ||
      (body.match(/debugSessionId="([^"]+)"/) || [])[1];

    if (debugSessionId) {
      this.sessionId = debugSessionId;
    }

    return this.sessionId;
  }

  /**
   * Execute a step action.
   * @param {'stepInto'|'stepOver'|'stepOut'|'stepReturn'|'continue'|'stepContinue'} type
   * @returns {Promise<{ position: object, source: string[] }>}
   */
  async step(type = 'stepOver') {
    // Map user-friendly names to ADT method names
    const methodMap = {
      stepInto: 'stepInto',
      stepOver: 'stepOver',
      stepOut: 'stepReturn',     // ADT uses stepReturn, not stepOut
      stepReturn: 'stepReturn',
      continue: 'stepContinue',  // ADT uses stepContinue, not continue
      stepContinue: 'stepContinue'
    };

    const validTypes = Object.keys(methodMap);
    if (!validTypes.includes(type)) {
      throw new Error(`Invalid step type: ${type}. Use one of: ${validTypes.join(', ')}`);
    }

    const method = methodMap[type];

    // stepContinue resumes the debuggee — it runs to the next breakpoint or to
    // completion.  When the program runs to completion ADT returns HTTP 500
    // (no suspended session left).  Treat both 200 and 500 as "continued".
    if (method === 'stepContinue') {
      return retryOnIcmError(async () => {
        try {
          await this.http.post(`/sap/bc/adt/debugger?method=${method}`, '', {
            contentType: 'application/vnd.sap.as+xml',
            headers: { ...STATEFUL_HEADER, 'Accept': 'application/xml' }
          });
          // 200: program hit another breakpoint (or is still running).
          // Position query is not meaningful until a new breakpoint fires via
          // the listener, so return the sentinel and let the caller re-attach.
          return { position: { continued: true }, source: [] };
        } catch (err) {
          // 500: debuggee ran to completion, session ended normally.
          if (err && err.statusCode === 500) {
            return { position: { continued: true, finished: true }, source: [] };
          }
          throw err;
        }
      });
    }

    return retryOnIcmError(async () => {
      await this.http.post(`/sap/bc/adt/debugger?method=${method}`, '', {
        contentType: 'application/vnd.sap.as+xml',
        headers: { ...STATEFUL_HEADER, 'Accept': 'application/xml' }
      });
      return this.getPosition();
    });
  }

  /**
   * Retrieve all (or named) variables at the current stack frame.
   *
   * Two-step protocol (from abap-adt-api v7):
   *   1. getChildVariables — body requests children of @ROOT, @PARAMETERS, @LOCALS,
   *      and @DATAAGING in a single call.  Content-Type/Accept must include
   *      dataname=com.sap.adt.debugger.ChildVariables (routing key for ADT handler).
   *      The response HIERARCHIES table maps parent→child IDs.
   *      Requesting @PARAMETERS and @LOCALS directly is required to get their
   *      leaf children (the actual variable IDs) in the same response.
   *   2. getVariables — body uses STPDA_ADT_VARIABLE with the leaf CHILD_IDs
   *      extracted from HIERARCHIES (children whose parent is @PARAMETERS or
   *      @LOCALS, not the group names themselves).
   *      Content-Type/Accept must include dataname=com.sap.adt.debugger.Variables.
   *
   * @param {string|null} name - Optional variable name filter (case-insensitive)
   * @returns {Promise<Array<{ name: string, type: string, value: string }>>}
   */
  async getVariables(name = null) {
    const CT_CHILD = 'application/vnd.sap.as+xml; charset=UTF-8; dataname=com.sap.adt.debugger.ChildVariables';
    const CT_VARS  = 'application/vnd.sap.as+xml; charset=UTF-8; dataname=com.sap.adt.debugger.Variables';

    // ── Step 1: getChildVariables ──────────────────────────────────────────
    // Request the hierarchy for all scopes in one call:
    //   @ROOT       → discovers group nodes: @PARAMETERS, @LOCALS, ME
    //   @PARAMETERS → actual import/export parameter variable IDs
    //   @LOCALS     → actual local variable IDs
    //   @DATAAGING  → internal data aging variables
    // Including @PARAMETERS and @LOCALS here is essential — ADT expands their
    // children in the same response so we get the leaf IDs in a single round-trip.
    const childBody =
      `<?xml version="1.0" encoding="UTF-8" ?>` +
      `<asx:abap version="1.0" xmlns:asx="http://www.sap.com/abapxml">` +
      `<asx:values><DATA><HIERARCHIES>` +
      `<STPDA_ADT_VARIABLE_HIERARCHY><PARENT_ID>@ROOT</PARENT_ID></STPDA_ADT_VARIABLE_HIERARCHY>` +
      `<STPDA_ADT_VARIABLE_HIERARCHY><PARENT_ID>@PARAMETERS</PARENT_ID></STPDA_ADT_VARIABLE_HIERARCHY>` +
      `<STPDA_ADT_VARIABLE_HIERARCHY><PARENT_ID>@LOCALS</PARENT_ID></STPDA_ADT_VARIABLE_HIERARCHY>` +
      `<STPDA_ADT_VARIABLE_HIERARCHY><PARENT_ID>@DATAAGING</PARENT_ID></STPDA_ADT_VARIABLE_HIERARCHY>` +
      `</HIERARCHIES></DATA></asx:values></asx:abap>`;

    let childXml = '';
    try {
      const resp = await this.http.post(
        '/sap/bc/adt/debugger?method=getChildVariables', childBody, {
          contentType: CT_CHILD,
          headers: { ...STATEFUL_HEADER, 'Accept': CT_CHILD }
        }
      );
      childXml = resp.body || '';
    } catch (e) {
      return [];
    }

    // Extract the child IDs whose parent is @PARAMETERS or @LOCALS.
    // These are the opaque IDs used in the next call — not human-readable names.
    const childIds = extractChildIds(childXml, ['@PARAMETERS', '@LOCALS', '@ROOT', '@DATAAGING']);

    if (childIds.length === 0) return [];

    // ── Step 2: getVariables — get values for those IDs ────────────────────
    const varBody =
      `<?xml version="1.0" encoding="UTF-8" ?>` +
      `<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">` +
      `<asx:values><DATA>` +
      childIds.map(id => `<STPDA_ADT_VARIABLE><ID>${id}</ID></STPDA_ADT_VARIABLE>`).join('') +
      `</DATA></asx:values></asx:abap>`;

    let varsXml = '';
    try {
      const resp = await this.http.post(
        '/sap/bc/adt/debugger?method=getVariables', varBody, {
          contentType: CT_VARS,
          headers: { ...STATEFUL_HEADER, 'Accept': CT_VARS }
        }
      );
      varsXml = resp.body || '';
    } catch (e) {
      // Fall back to whatever step 1 returned
      varsXml = childXml;
    }

    const variables = parseVariables(varsXml);
    if (name) {
      return variables.filter(v => v.name.toUpperCase() === name.toUpperCase());
    }
    return variables;
  }

  /**
   * Drill one level into a complex variable (internal table rows, structure fields).
   *
   * For structures: uses getChildVariables with parentId — returns field child IDs.
   * For internal tables: uses getChildVariables with TABLE_FROM/TABLE_TO range because
   *   ADT does not populate HIERARCHY entries for tables without an explicit range.
   *
   * @param {string} parentId  - Opaque variable ID returned in the `id` field of getVariables()
   * @param {object} [meta]    - Optional metadata from the parent variable: { metaType, tableLines }
   * @returns {Promise<Array<{ id: string, name: string, type: string, value: string }>>}
   */
  async getVariableChildren(parentId, meta = {}) {
    const CT_VARS  = 'application/vnd.sap.as+xml; charset=UTF-8; dataname=com.sap.adt.debugger.Variables';
    const CT_CHILD = 'application/vnd.sap.as+xml; charset=UTF-8; dataname=com.sap.adt.debugger.ChildVariables';

    const isTable    = meta.metaType === 'table';
    const tableLines = meta.tableLines || 0;

    // ── Data reference (TYPE REF TO): dereference with parentId->* ───────
    // ADT represents the pointed-to object as a single virtual variable whose
    // ID is "<parentId>->*".  Fetching it with getVariables returns the
    // dereferenced structure/scalar.  If that variable is itself a structure,
    // the caller can expand it again.
    // Example: LR_REQUEST (meta=dataref) → LR_REQUEST->* (the TY_UNIT_PARAMS struct)
    if (meta.metaType === 'dataref') {
      const derefId = `${parentId}->*`;
      const varBody =
        `<?xml version="1.0" encoding="UTF-8" ?>` +
        `<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">` +
        `<asx:values><DATA>` +
        `<STPDA_ADT_VARIABLE><ID>${derefId}</ID></STPDA_ADT_VARIABLE>` +
        `</DATA></asx:values></asx:abap>`;

      try {
        const resp = await this.http.post(
          '/sap/bc/adt/debugger?method=getVariables', varBody, {
            contentType: CT_VARS,
            headers: { ...STATEFUL_HEADER, 'Accept': CT_VARS }
          }
        );
        const derefVars = parseVariables(resp.body || '');
        // If the dereference returned a structure, expand one level into it
        if (derefVars.length === 1 && derefVars[0].metaType === 'structure') {
          return this.getVariableChildren(derefVars[0].id || derefId, { metaType: 'structure' });
        }
        return derefVars;
      } catch (e) {
        return [];
      }
    }

    // ── Internal table: rows use ID format parentId[N] ────────────────────
    // ADT getChildVariables does not return row hierarchy entries for tables
    // (even with TABLE_FROM/TABLE_TO in the request body).
    // Instead, rows can be fetched directly with getVariables using IDs of
    // the form  LT_PARTS[1], LT_PARTS[2], ... LT_PARTS[N].
    if (isTable && tableLines > 0) {
      const limit  = Math.min(tableLines, 100);
      const rowIds = [];
      for (let i = 1; i <= limit; i++) rowIds.push(`${parentId}[${i}]`);

      const varBody =
        `<?xml version="1.0" encoding="UTF-8" ?>` +
        `<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">` +
        `<asx:values><DATA>` +
        rowIds.map(id => `<STPDA_ADT_VARIABLE><ID>${id}</ID></STPDA_ADT_VARIABLE>`).join('') +
        `</DATA></asx:values></asx:abap>`;

      try {
        const resp = await this.http.post(
          '/sap/bc/adt/debugger?method=getVariables', varBody, {
            contentType: CT_VARS,
            headers: { ...STATEFUL_HEADER, 'Accept': CT_VARS }
          }
        );
        return parseVariables(resp.body || '');
      } catch (e) {
        return [];
      }
    }

    // ── Structure / object reference: use getChildVariables ───────────────
    const childBody =
      `<?xml version="1.0" encoding="UTF-8" ?>` +
      `<asx:abap version="1.0" xmlns:asx="http://www.sap.com/abapxml">` +
      `<asx:values><DATA><HIERARCHIES>` +
      `<STPDA_ADT_VARIABLE_HIERARCHY><PARENT_ID>${parentId}</PARENT_ID></STPDA_ADT_VARIABLE_HIERARCHY>` +
      `</HIERARCHIES></DATA></asx:values></asx:abap>`;

    let childXml = '';
    try {
      const resp = await this.http.post(
        '/sap/bc/adt/debugger?method=getChildVariables', childBody, {
          contentType: CT_CHILD,
          headers: { ...STATEFUL_HEADER, 'Accept': CT_CHILD }
        }
      );
      childXml = resp.body || '';
    } catch (e) {
      return [];
    }

    const childIds = extractChildIds(childXml, [parentId]);
    if (childIds.length === 0) return [];

    const varBody =
      `<?xml version="1.0" encoding="UTF-8" ?>` +
      `<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">` +
      `<asx:values><DATA>` +
      childIds.map(id => `<STPDA_ADT_VARIABLE><ID>${id}</ID></STPDA_ADT_VARIABLE>`).join('') +
      `</DATA></asx:values></asx:abap>`;

    try {
      const resp = await this.http.post(
        '/sap/bc/adt/debugger?method=getVariables', varBody, {
          contentType: CT_VARS,
          headers: { ...STATEFUL_HEADER, 'Accept': CT_VARS }
        }
      );
      return parseVariables(resp.body || '');
    } catch (e) {
      return [];
    }
  }

  /**
   * @returns {Promise<Array<{ frame: number, class: string, method: string, line: number }>>}
   */
  async getStack() {
    return retryOnIcmError(async () => {
      // Try newer dedicated stack endpoint first (abap-adt-api v7+ approach)
      try {
        const { body } = await this.http.get(
          '/sap/bc/adt/debugger/stack?emode=_&semanticURIs=true', {
            accept: 'application/xml',
            headers: STATEFUL_HEADER
          }
        );
        const frames = parseStack(body);
        if (frames.length > 0) return frames;
      } catch (e) {
        // Fall through to POST approach for any GET failure (including 400 on systems
        // that don't support the dedicated /debugger/stack endpoint)
      }
      // Fallback: POST approach (older ADT versions)
      const { body } = await this.http.post(
        '/sap/bc/adt/debugger?method=getStack&emode=_&semanticURIs=true', '', {
          contentType: 'application/vnd.sap.as+xml',
          headers: { ...STATEFUL_HEADER, 'Accept': 'application/xml' }
        }
      );
      return parseStack(body);
    });
  }

  /**
   * Get the current execution position (top frame of call stack).
   * Also fetches source lines around the current line.
   * @returns {Promise<{ position: object, source: string[] }>}
   */
  async getPosition() {
    const frames = await this.getStack();
    const position = frames[0] || {};
    let source = [];

    if (position.line) {
      try {
        source = await this.getSource(position);
      } catch (e) {
        // Source fetch is best-effort
      }
    }

    return { position, source };
  }

  /**
   * Fetch source lines around the current line for a given stack frame.
   *
   * Strategy:
   *   1. Try ADT via the adtUri embedded in the stack frame
   *      (e.g. /sap/bc/adt/oo/classes/zcl_foo/source/main)
   *   2. Fall back to local file system — search for a matching .abap file
   *      in the current working directory tree.
   *
   * @param {object} frame  - Stack frame from parseStack() (must have .line, optionally .adtUri, .program)
   * @param {number} [context=5] - Lines of context above and below current line
   * @returns {Promise<Array<{ lineNumber: number, text: string, current: boolean }>>}
   */
  async getSource(frame, context = 5) {
    const line = frame.line || 1;
    const start = Math.max(1, line - context);
    const end = line + context;

    // ── 1. Try ADT source via adtUri ──────────────────────────────────────
    if (frame.adtUri) {
      // adtUri may contain a fragment (#start=25,0) — strip it
      const adtPath = frame.adtUri.split('#')[0];
      try {
        const { body } = await this.http.get(
          `${adtPath}?start=${start}&end=${end}`,
          { accept: 'text/plain' }
        );
        if (body && body.trim()) {
          const allLines = parseSourceLines(body, 1, line);
          // ADT may ignore range params and return the full file — slice to window
          if (allLines.length > (end - start + 2)) {
            return allLines.slice(start - 1, end);
          }
          return allLines;
        }
      } catch (e) {
        // Fall through to local file
      }
    }

    // ── 2. Try local file ─────────────────────────────────────────────────
    const localFile = resolveLocalFile(frame.program || '');
    if (localFile) {
      try {
        const content = fs.readFileSync(localFile, 'utf8');
        const lines = content.split('\n');
        const result = [];
        for (let n = start; n <= Math.min(end, lines.length); n++) {
          result.push({ lineNumber: n, text: lines[n - 1] || '', current: n === line });
        }
        return result;
      } catch (e) {
        // File not readable
      }
    }

    return [];
  }

  /**
   * Expand a variable along a `->` separated path.
   *
   * Allows users to drill through nested object/structure/table levels in one
   * command:  `x LO_FACTORY->MT_COMMAND_MAP`
   *
   * Algorithm:
   *   1. Start with all top-level variables.
   *   2. For each path segment after the first:
   *      a. Find the parent variable by matching the previous segment name
   *      b. Expand it to get its children
   *      c. Find the child matching the next segment name
   *   3. Expand the final target and return its children.
   *
   * @param {string[]} pathParts  - e.g. ['LO_FACTORY', 'MT_COMMAND_MAP']
   * @returns {Promise<{ variable: object, children: object[] }>}
   *   variable — the resolved target variable (at the end of the path)
   *   children — the expanded children of that target
   */
  async expandPath(pathParts) {
    if (pathParts.length === 0) throw new Error('Empty path');

    // Resolve root variable from top-level vars
    let vars = await this.getVariables();
    let current = vars.find(v => v.name.toUpperCase() === pathParts[0].toUpperCase());
    if (!current) {
      throw new Error(`Variable '${pathParts[0]}' not found. Run 'v' to list variables.`);
    }

    // Walk intermediate path segments
    for (let i = 1; i < pathParts.length; i++) {
      if (!current.id) {
        throw new Error(`Variable '${current.name}' has no ADT ID — cannot expand.`);
      }
      const meta = { metaType: current.metaType || '', tableLines: current.tableLines || 0 };
      const children = await this.getVariableChildren(current.id, meta);
      const segment = pathParts[i].toUpperCase();

      // * — explicit dereference marker (ABAP/ADT convention: TYPE REF TO -> *)
      // getVariableChildren already auto-derefs datarefs, so * is a no-op here.
      // It is accepted to allow: x lr_request->* and x lr_request->*->files
      if (segment === '*') continue;

      // Pattern 1: FIELD[N] — find FIELD in children, expand it, take row N
      // Supports: x LO_FACTORY->MT_COMMAND_MAP[1]
      const fieldRowMatch = segment.match(/^([A-Z0-9_@]+)\[(\d+)\]$/);
      if (fieldRowMatch) {
        const fieldName = fieldRowMatch[1];
        const rowIndex  = parseInt(fieldRowMatch[2], 10);
        const fieldVar  = children.find(c => c.name.toUpperCase() === fieldName);
        if (!fieldVar) {
          const avail = children.map(c => c.name).join(', ') || '(none)';
          throw new Error(`'${fieldName}' not found in '${current.name}'. Available: ${avail}`);
        }
        if (!fieldVar.id) {
          throw new Error(`'${fieldVar.name}' has no ADT ID — cannot expand.`);
        }
        const rowMeta = { metaType: fieldVar.metaType || '', tableLines: fieldVar.tableLines || 0 };
        const rows = await this.getVariableChildren(fieldVar.id, rowMeta);
        const rowTarget = rows.find(r => r.name === `[${rowIndex}]`);
        if (!rowTarget) {
          throw new Error(
            `Row [${rowIndex}] not found in '${fieldName}'. Table has ${rows.length} rows.`
          );
        }
        current = rowTarget;
        continue;
      }

      // Pattern 2: [N] — take row N from current (a table expanded in previous step)
      // Supports: x LO_FACTORY->MT_COMMAND_MAP->[1]
      const rowOnlyMatch = segment.match(/^\[(\d+)\]$/);
      if (rowOnlyMatch) {
        const rowTarget = children.find(r => r.name === `[${rowOnlyMatch[1]}]`);
        if (!rowTarget) {
          throw new Error(
            `Row [${rowOnlyMatch[1]}] not found. Table has ${children.length} rows.`
          );
        }
        current = rowTarget;
        continue;
      }

      // Pattern 3: plain field name
      const next = children.find(c => c.name.toUpperCase() === segment);
      if (!next) {
        const names = children.map(c => c.name).join(', ') || '(none)';
        throw new Error(
          `'${pathParts[i]}' not found in '${current.name}'. Available: ${names}`
        );
      }
      current = next;
    }

    if (!current.id) {
      throw new Error(`Variable '${current.name}' has no ADT ID — cannot expand.`);
    }

    const meta = { metaType: current.metaType || '', tableLines: current.tableLines || 0 };
    const children = await this.getVariableChildren(current.id, meta);
    return { variable: current, children };
  }

  /**
   * Terminate the debug session.
   * Retries on transient ICM 400 errors so the ABAP work process is reliably
   * released even when the system is under load (e.g. during test:all).
   */
  async terminate() {
    await retryOnIcmError(async () => {
      await this.http.post('/sap/bc/adt/debugger?method=terminateDebuggee', '', {
        contentType: 'application/vnd.sap.as+xml',
        headers: STATEFUL_HEADER
      });
    });
  }

  /**
   * Detach from the debuggee without killing it.
   * Issues a stepContinue so the ABAP program resumes running.
   *
   * stepContinue is a long-poll in ADT — it only responds when the program
   * hits another breakpoint (200) or finishes (500), which may be never.
   * We race the POST against an 8-second timeout: if ADT responds quickly
   * (program finished → HTTP 500, treated as success) we return early;
   * otherwise the timeout fires, which is long enough for the TCP layer to
   * have delivered the request to ADT (even on a loaded system) and for the
   * ABAP WP to have been released, preventing it from staying frozen in SM50.
   */
  async detach() {
    try {
      const postPromise = this.http.post('/sap/bc/adt/debugger?method=stepContinue', '', {
        contentType: 'application/vnd.sap.as+xml',
        headers: { ...STATEFUL_HEADER, 'Accept': 'application/xml' }
      }).catch(err => {
        // 500 means program ran to completion — session is released, not an error.
        if (err && err.statusCode === 500) return null;
        // Any other error: session already gone, ignore.
      });
      const timeout = new Promise(resolve => setTimeout(resolve, 8000));
      await Promise.race([postPromise, timeout]);
    } catch (e) {
      // Ignore — session may have already closed.
    }
  }
}

// ─── XML parsers ─────────────────────────────────────────────────────────────

/**
 * Extract a human-readable name from an opaque ADT object-reference child ID.
 *
 * ADT returns child variables of object references with opaque IDs as their
 * NAME field, e.g.:
 *   {O:73*\CLASS=ZCL_ABGAGT_CMD_FACTORY}-MT_COMMAND_MAP\TYPE=%_T000...
 *   {O:73*\CLASS=ZCL_ABGAGT_CMD_FACTORY}-MT_COMMAND_MAP[1]
 *
 * Patterns handled (in priority order):
 *   {O:N*\...}-FIELDNAME[N]          → [N]      (table row)
 *   {O:N*\...}-FIELDNAME\TYPE=...    → FIELDNAME (object attribute)
 *   {O:N*\...}-FIELDNAME             → FIELDNAME (object attribute, no type suffix)
 *
 * Returns null when the input is already a friendly name (no opaque prefix).
 *
 * @param {string} rawName
 * @returns {string|null}
 */
function extractFriendlyName(rawName) {
  if (!rawName) return null;

  if (rawName.startsWith('{')) {
    // Structure field inside a table row: {O:73*\...}-TABLE[N]-FIELDNAME → FIELDNAME
    // This must be checked first (most specific pattern).
    const rowFieldMatch = rawName.match(/\}-[A-Z0-9_@~]+\[\d+\]-([A-Z0-9_@~]+)(?:\\TYPE=|$)/i);
    if (rowFieldMatch) return rowFieldMatch[1].toUpperCase();
    // Table row suffix: {O:73*\...}-TABLE[N] → [N]
    const rowMatch = rawName.match(/\}-[A-Z0-9_@~]+(\[\d+\])\s*$/i);
    if (rowMatch) return rowMatch[1];
    // Object attribute: {O:73*\...}-IF_FOO~FIELDNAME or -FIELDNAME → IF_FOO~FIELDNAME / FIELDNAME
    // ~ is the ABAP interface attribute separator; keep it so interface-redefined constants
    // remain distinguishable (e.g. IF_HTTP_ENTITY~FORMFIELD_ENCODING vs IF_HTTP_REQUEST~...).
    const attrMatch = rawName.match(/\}-([A-Z0-9_@~]+)(?:\\TYPE=|$)/i);
    return attrMatch ? attrMatch[1].toUpperCase() : null;
  }

  // Dereference field access: VARNAME->FIELDNAME or VARNAME->FIELD[N] → FIELD / [N]
  // This covers children of dereferenced data references, e.g.
  //   LR_REQUEST->PACKAGE   → PACKAGE
  //   LR_REQUEST->FILES     → FILES
  //   LR_REQUEST->FILES[1]  → [1]   (table row within dereferenced field)
  // The last ->FIELDNAME or ->FIELD[N] segment is used.
  // Exclude ->* (the dereference marker itself, not a field name).
  if (rawName.includes('->')) {
    // Table row: VARNAME->FIELD[N] → [N]  (more specific — check first)
    const derefRowMatch = rawName.match(/->([A-Z0-9_@]+)(\[\d+\])\s*$/i);
    if (derefRowMatch) return derefRowMatch[2];
    // Plain field: VARNAME->FIELDNAME → FIELDNAME
    const derefFieldMatch = rawName.match(/->([A-Z0-9_@]+)(?:\\TYPE=|\s*$)/i);
    if (derefFieldMatch && derefFieldMatch[1] !== '*') return derefFieldMatch[1].toUpperCase();
  }

  return null;
}

/**
 * Clean an ADT type string for human display.
 *
 * ADT returns several non-friendly type formats for object/table children:
 *   \TYPE=%_T00004S00000130...        — generated internal type ID (starts with %) → return ''
 *   \TYPE=TY_UNIT_PARAMS             — named type → return 'TY_UNIT_PARAMS'
 *   Type TY_COMMAND_MAP in ZCL_...   — verbose format → return 'TY_COMMAND_MAP'
 *
 * @param {string} rawType
 * @returns {string}
 */
function cleanTypeStr(rawType) {
  if (!rawType) return '';
  if (rawType.startsWith('\\TYPE=')) {
    const inner = rawType.slice(6);  // strip the \TYPE= prefix
    // Internal generated type IDs start with %, = or are empty — discard them
    if (!inner || inner.startsWith('%') || inner.startsWith('=') || inner.startsWith(' ')) return '';
    return inner;  // named type like TY_UNIT_PARAMS — keep it
  }
  // "Type TY_COMMAND_MAP in ZCL_ABGAGT_CMD_FACTORY" → "TY_COMMAND_MAP"
  const m = rawType.match(/^Type\s+(\S+)/i);
  if (m) return m[1];
  return rawType;
}

/**
 * Decode XML character entity references in a string.
 * ADT XML-encodes variable names, types, and values — e.g. field symbol names
 * appear as &lt;LS_REQUEST&gt; instead of <LS_REQUEST>.
 *
 * @param {string} s
 * @returns {string}
 */
function decodeXmlEntities(s) {
  if (!s || !s.includes('&')) return s;
  return s
    .replace(/&lt;/g,   '<')
    .replace(/&gt;/g,   '>')
    .replace(/&amp;/g,  '&')
    .replace(/&quot;/g, '"')
    .replace(/&apos;/g, "'");
}

/**
 * Parse ADT variables response XML into a flat array.
 * Handles the actual SAP ADT format (STPDA_ADT_VARIABLE elements) and
 * the legacy namespace-prefixed format (adtdbg:variable or dbg:variable).
 *
 * @param {string} xml
 * @returns {Array<{ name: string, type: string, value: string }>}
 */
function parseVariables(xml) {
  const variables = [];

  // Try modern SAP ADT format: <STPDA_ADT_VARIABLE> elements
  const stpdaRe = /<STPDA_ADT_VARIABLE>([\s\S]*?)<\/STPDA_ADT_VARIABLE>/gi;
  let m;
  while ((m = stpdaRe.exec(xml)) !== null) {
    const inner = m[1];
    const id         = AdtHttp.extractXmlAttr(inner, 'ID',                 null) || '';
    const rawName    = decodeXmlEntities(AdtHttp.extractXmlAttr(inner, 'NAME',               null) || '');
    const name       = extractFriendlyName(rawName) || rawName;
    const type       = cleanTypeStr(decodeXmlEntities(AdtHttp.extractXmlAttr(inner, 'DECLARED_TYPE_NAME', null) ||
                       AdtHttp.extractXmlAttr(inner, 'ACTUAL_TYPE_NAME',    null) || ''));
    const value      = decodeXmlEntities(AdtHttp.extractXmlAttr(inner, 'VALUE',              null) || '');
    const metaType   = AdtHttp.extractXmlAttr(inner, 'META_TYPE',          null) || '';
    const tableLines = parseInt(AdtHttp.extractXmlAttr(inner, 'TABLE_LINES', null) || '0', 10);
    if (name) variables.push({ id, name, type, value, metaType, tableLines });
  }

  if (variables.length > 0) return variables;

  // Fallback: legacy namespace-prefixed format
  const entryRe = /<(?:adtdbg:|dbg:)?variable([^>]*)>([\s\S]*?)<\/(?:adtdbg:|dbg:)?variable>/gi;
  let entryMatch;
  while ((entryMatch = entryRe.exec(xml)) !== null) {
    const attrs = entryMatch[1];
    const inner = entryMatch[2];
    const name  = decodeXmlEntities((attrs.match(/(?:dbg:|adtdbg:)?name="([^"]*)"/)  || [])[1] ||
                  AdtHttp.extractXmlAttr(inner, 'name',  null) || '');
    const type  = decodeXmlEntities((attrs.match(/(?:dbg:|adtdbg:)?type="([^"]*)"/)  || [])[1] ||
                  AdtHttp.extractXmlAttr(inner, 'type',  null) || '');
    const value = decodeXmlEntities((attrs.match(/(?:dbg:|adtdbg:)?value="([^"]*)"/) || [])[1] ||
                  AdtHttp.extractXmlAttr(inner, 'value', null) || '');
    if (name) variables.push({ name, type, value });
  }

  return variables;
}

/**
 * Parse ADT stack response XML into a flat array of frames.
 * The response uses <stackEntry> elements with plain attributes:
 *   stackPosition, programName, includeName, line, eventName, stackType
 * @param {string} xml
 * @returns {Array<{ frame: number, class: string, method: string, include: string, line: number, program: string }>}
 */
function parseStack(xml) {
  const frames = [];

  // Match self-closing <stackEntry .../> or wrapped <stackEntry ...></stackEntry>
  const entryRe = /<stackEntry([^>]*?)(?:\/>|>[\s\S]*?<\/stackEntry>)/gi;
  let m;

  while ((m = entryRe.exec(xml)) !== null) {
    const attrs = m[1];

    const stackType = (attrs.match(/\bstackType="([^"]*)"/) || [])[1] || '';
    // Skip DYNP (screen) frames — only ABAP frames are useful
    if (stackType && stackType !== 'ABAP') continue;

    const frameNum = parseInt((attrs.match(/\bstackPosition="([^"]*)"/) || [])[1] || '0', 10);
    const cls      = (attrs.match(/\bprogramName="([^"]*)"/)  || [])[1] || '';
    const method   = (attrs.match(/\beventName="([^"]*)"/)    || [])[1] || '';
    const include  = (attrs.match(/\bincludeName="([^"]*)"/)  || [])[1] || '';
    const lineStr  = (attrs.match(/\bline="([^"]*)"/)         || [])[1] || '0';
    const line     = parseInt(lineStr, 10);
    const isActive = (attrs.match(/\bisActive="([^"]*)"/)     || [])[1] === 'true';
    // adtcore:uri="/sap/bc/adt/oo/classes/zcl_foo/source/main#start=25,0"
    const adtUri   = (attrs.match(/adtcore:uri="([^"]*)"/)    || [])[1] || null;

    frames.push({ frame: frameNum, class: cls, method, include, line, program: cls, isActive, adtUri });
  }

  // Sort descending by stackPosition so frame[0] = top of stack (highest position)
  frames.sort((a, b) => b.frame - a.frame);
  return frames;
}

// ─── Source helpers ──────────────────────────────────────────────────────────

/**
 * Convert a raw ADT source body string into the structured line array.
 * @param {string} body     - Raw text from ADT (may have leading/trailing whitespace)
 * @param {number} start    - Line number of the first line in body
 * @param {number} current  - The currently executing line number
 */
function parseSourceLines(body, start, current) {
  return body.split('\n').map((text, i) => ({
    lineNumber: start + i,
    text,
    current: (start + i) === current
  }));
}

/**
 * Try to find a local .abap file matching the ABAP program/class name.
 *
 * The programName from the stack is in the form:
 *   ZCL_ABGAGT_UTIL===============CP  (class main pool)
 *   SAPLHTTP_RUNTIME                  (program)
 *
 * We extract the base object name, lowercase it, and glob for matching
 * .abap files under the current working directory.
 *
 * @param {string} programName - ABAP program name (with optional padding/suffix)
 * @returns {string|null} Absolute path to the local file, or null if not found
 */
function resolveLocalFile(programName) {
  if (!programName) return null;

  // Strip padding characters and trailing include suffix (CP, CU, etc.)
  const base = programName
    .replace(/=+\w*$/, '')   // remove trailing ===...CP, ===...CM009 etc.
    .replace(/=+$/, '')       // remove any remaining padding
    .trim();

  if (!base) return null;

  const baseLower = base.toLowerCase();
  const cwd = process.cwd();

  // Candidate patterns in priority order: class main, locals, program, interface
  const patterns = [
    `${baseLower}.clas.abap`,
    `${baseLower}.clas.locals_imp.abap`,
    `${baseLower}.prog.abap`,
    `${baseLower}.intf.abap`,
  ];

  // Search recursively under common source dirs, then cwd
  const searchDirs = [];
  for (const dir of ['abap', 'src', '.']) {
    const d = path.join(cwd, dir);
    try { if (fs.statSync(d).isDirectory()) searchDirs.push(d); } catch (e) { /* skip */ }
  }

  for (const dir of searchDirs) {
    for (const pattern of patterns) {
      const candidate = path.join(dir, pattern);
      try {
        if (fs.existsSync(candidate)) return candidate;
      } catch (e) { /* skip */ }
    }
  }

  return null;
}

/**
 * Extract CHILD_ID values from a getChildVariables HIERARCHIES response.
 * Returns IDs whose PARENT_ID is one of the given parent groups.
 * Skips virtual group IDs (starting with @) — those are containers, not real variables.
 *
 * @param {string} xml       - Response body from getChildVariables
 * @param {string[]} parents - Parent group IDs to include (e.g. ['@PARAMETERS','@LOCALS','@ROOT'])
 * @returns {string[]}
 */
function extractChildIds(xml, parents) {
  const ids = [];
  const re = /<STPDA_ADT_VARIABLE_HIERARCHY>([\s\S]*?)<\/STPDA_ADT_VARIABLE_HIERARCHY>/gi;
  let m;
  while ((m = re.exec(xml)) !== null) {
    const inner = m[1];
    const parentId = (inner.match(/<PARENT_ID>([^<]*)<\/PARENT_ID>/) || [])[1] || '';
    const childId  = (inner.match(/<CHILD_ID>([^<]*)<\/CHILD_ID>/)   || [])[1] || '';
    if (parents.includes(parentId) && childId && !childId.startsWith('@')) {
      ids.push(childId);
    }
  }
  // Deduplicate (same ID may appear under multiple parents)
  return [...new Set(ids)];
}

module.exports = { DebugSession, parseVariables, parseStack, extractFriendlyName };
