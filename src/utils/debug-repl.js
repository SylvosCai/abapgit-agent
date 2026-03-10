'use strict';

/**
 * Interactive readline REPL for human debug mode.
 * Entered when `debug attach` is called without --json.
 */
const readline = require('readline');
const { printVarList } = require('./debug-render');

const HELP = `
  Commands:
    s  / step          — Step into
    n  / next          — Step over
    o  / out           — Step out
    c  / continue      — Continue execution
    v  / vars          — Show variables
    x  / expand <var>  — Drill into a complex variable (table / structure)
    bt / stack         — Show call stack
    q  / quit          — Detach debugger (program continues running)
    kill               — Terminate the running program (hard abort)
    h  / help          — Show this help
`;

/**
 * Render the current debugger state to the terminal.
 * @param {object} position - { class, method, include, line, ... }
 * @param {Array}  source   - [{ lineNumber, text, current }]
 * @param {Array}  variables - [{ name, type, value }]
 */
function renderState(position, source, variables) {
  const where = position.class
    ? `${position.class}->${position.method}`
    : (position.method || position.program || '?');
  const lineRef = position.line ? ` (line ${position.line})` : '';

  console.log(`\n  ABAP Debugger — ${where}${lineRef}`);
  console.log('  ' + '─'.repeat(55));

  if (source && source.length > 0) {
    source.forEach(({ lineNumber, text, current }) => {
      const marker = current ? '>' : ' ';
      console.log(`  ${marker}${String(lineNumber).padStart(4)}  ${text}`);
    });
  }

  if (variables && variables.length > 0) {
    printVarList(variables);
  }

  console.log('\n  [s]tep  [n]ext  [o]ut  [c]ontinue  [v]ars  [x]pand  [bt]  [q]uit(detach)  kill');
}

/**
 * Start the interactive REPL.
 * @param {import('./debug-session').DebugSession} session
 * @param {{ position: object, source: string[] }} initialState
 * @param {Function} [onBeforeExit]  Optional async cleanup called before process.exit(0)
 */
async function startRepl(session, initialState, onBeforeExit) {
  let { position, source } = initialState;
  let variables = [];
  let exitCleanupDone = false;
  async function runExitCleanup() {
    if (exitCleanupDone) return;
    exitCleanupDone = true;
    if (onBeforeExit) {
      try { await onBeforeExit(); } catch (e) { /* ignore */ }
    }
  }

  try {
    variables = await session.getVariables();
  } catch (e) {
    // Best-effort
  }

  renderState(position, source, variables);

  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: '\n  debug> '
  });

  rl.prompt();

  /**
   * Returns true when the step result contains a meaningful program position.
   * After `continue` (or a step that runs off the end of a method), ADT returns
   * an empty stack, leaving position.class / .method / .program all undefined.
   * That signals the debuggee has completed — no further stepping is possible.
   */
  function _hasPosition(pos) {
    return pos && (pos.class || pos.method || pos.program);
  }

  /**
   * Common handler for step results.
   * Returns true if the session ended (caller should close the REPL).
   */
  async function _handleStepResult(result) {
    position = result.position;
    source   = result.source;
    if (!_hasPosition(position)) {
      console.log('\n  Execution completed — no active breakpoint. Debug session ended.\n');
      // Program already finished — nothing to terminate or detach; just close.
      rl.close();
      return true;
    }
    variables = await session.getVariables().catch(() => []);
    renderState(position, source, variables);
    return false;
  }

  rl.on('line', async (line) => {
    const cmd = line.trim().toLowerCase();

    try {
      if (cmd === 's' || cmd === 'step') {
        if (await _handleStepResult(await session.step('stepInto'))) return;

      } else if (cmd === 'n' || cmd === 'next') {
        if (await _handleStepResult(await session.step('stepOver'))) return;

      } else if (cmd === 'o' || cmd === 'out') {
        if (await _handleStepResult(await session.step('stepOut'))) return;

      } else if (cmd === 'c' || cmd === 'continue') {
        console.log('\n  Continuing execution...');
        if (await _handleStepResult(await session.step('continue'))) return;

      } else if (cmd === 'v' || cmd === 'vars') {
        variables = await session.getVariables();
        printVarList(variables);

      } else if (cmd.startsWith('x ') || cmd.startsWith('expand ')) {
        const varExpr = cmd.replace(/^(x|expand)\s+/i, '').trim().toUpperCase();
        if (!varExpr) {
          console.log('  Usage: x <variable-name>  or  x <parent>-><child>');
        } else {
          // Normalize ABAP-style field accessors to use -> separator.
          const normalizedExpr = varExpr
            .replace(/\](-(?!>))/g, ']->') // [N]-FIELD → [N]->FIELD
            .replace(/\*(-(?!>))/g, '*->'); // *-FIELD   → *->FIELD
          const pathParts = normalizedExpr.split('->').map(s => s.replace(/^-+|-+$/g, '').trim()).filter(Boolean);

          if (pathParts.length > 1) {
            // Multi-segment path — use session.expandPath
            try {
              const { variable: target, children } = await session.expandPath(pathParts);
              _renderChildren(varExpr, target, children);
            } catch (err) {
              console.log(`\n  Error: ${err.message}`);
            }
          } else {
            // Single segment — find in last-fetched vars list
            const target = variables.find(v => v.name.toUpperCase() === varExpr);
            if (!target) {
              console.log(`\n  Variable '${varExpr}' not found. Run 'v' to list variables.`);
            } else if (!target.id) {
              console.log(`\n  Variable '${varExpr}' has no ADT ID — cannot expand.`);
            } else {
              const meta = { metaType: target.metaType || '', tableLines: target.tableLines || 0 };
              const children = await session.getVariableChildren(target.id, meta);
              _renderChildren(varExpr, target, children);
            }
          }
        }


      } else if (cmd === 'bt' || cmd === 'stack') {
        const frames = await session.getStack();
        console.log('\n  Call Stack:');
        frames.forEach(({ frame, class: cls, method, line }) => {
          const loc = cls ? `${cls}->${method}` : method;
          console.log(`    ${String(frame).padStart(3)}  ${loc}  (line ${line})`);
        });

      } else if (cmd === 'q' || cmd === 'quit') {
        console.log('\n  Detaching debugger — program will continue running...');
        try {
          await session.detach();
        } catch (e) {
          // Ignore detach errors
        }
        // Clear state AFTER detach so session 2 (takeover) exits via state-file check.
        await runExitCleanup();
        rl.close();
        return;

      } else if (cmd === 'kill') {
        console.log('\n  Terminating program (hard abort)...');
        try {
          await session.terminate();
        } catch (e) {
          // Ignore terminate errors
        }
        await runExitCleanup();
        rl.close();
        return;

      } else if (cmd === 'h' || cmd === 'help' || cmd === '?') {
        console.log(HELP);

      } else if (cmd !== '') {
        console.log(`  Unknown command: ${cmd}. Type 'h' for help.`);
      }
    } catch (err) {
      console.error(`\n  Error: ${err.message || JSON.stringify(err)}`);
    }

    rl.prompt();
  });

  rl.on('close', async () => {
    // stdin closed (EOF or Ctrl+D) without an explicit 'q' or 'kill' command.
    // Detach so the ABAP work process is released — without this the WP stays
    // frozen in SM50 (e.g. when attach is started with </dev/null stdin and
    // readline gets immediate EOF before the user can type a command).
    if (!exitCleanupDone) {
      try { await session.detach(); } catch (e) { /* ignore */ }
    }
    await runExitCleanup();
    // Drain stdout/stderr before exiting so the OS TCP stack has flushed the
    // outbound stepContinue request bytes. process.exit() tears down file
    // descriptors immediately — without this the WP can stay frozen if the
    // socket buffer hasn't been written to the NIC yet.
    await new Promise(resolve => {
      if (process.stdout.writableEnded) return resolve();
      process.stdout.write('', resolve);
    });
    process.exit(0);
  });

  return new Promise((resolve) => {
    rl.on('close', resolve);
  });
}

/**
 * Render the children of an expand operation.
 * Tables show a row-count hint and a path hint for further expansion.
 *
 * @param {string} expr     - The expression the user typed (e.g. 'LO_FACTORY->MT_COMMAND_MAP')
 * @param {object} variable - The expanded variable metadata
 * @param {Array}  children - The children returned by getVariableChildren / expandPath
 */
function _renderChildren(expr, variable, children) {
  if (children.length === 0) {
    console.log(`\n  ${expr} — no children (scalar or empty).`);
    return;
  }

  // Compute column widths from actual data (min 4/4, capped at 50/25).
  const nameW = Math.min(50, Math.max(4, ...children.map(c => (c.name || '').length)));
  const typeW = Math.min(25, Math.max(4, ...children.map(c => (c.type || c.metaType || '').length)));

  console.log(`\n  ${expr} (${(variable.type || variable.metaType || '?').toLowerCase()}):`);
  console.log('  ' + 'Name'.padEnd(nameW + 2) + 'Type'.padEnd(typeW + 2) + 'Value');
  console.log('  ' + '-'.repeat(nameW + typeW + 24));
  children.forEach(({ name, type, value, metaType, tableLines }) => {
    const displayType  = (type || metaType || '').toLowerCase().slice(0, typeW);
    const displayName  = name.toLowerCase();
    const displayValue = metaType === 'table'
      ? `[${tableLines} rows] — use 'x ${expr}->${displayName}' to expand`
      : value;
    console.log('  ' + displayName.padEnd(nameW + 2) + displayType.padEnd(typeW + 2) + displayValue);
  });
}

module.exports = { startRepl, renderState };
