'use strict';

/**
 * Shared display helpers for the debug command and REPL.
 */

/**
 * Format and print a variable list with dynamic column widths.
 *
 * Name display:
 *   {DATAAGING_TEMPERATURE_DEFAULT} → DATAAGING_TEMPERATURE_DEFAULT  (strip braces)
 *
 * Value display:
 *   {O:N*\CLASS=FOO}           → (FOO)         object reference
 *   {O:INITIAL}                → (null)         null object reference
 *   {A:N*\CLASS=FOO\TYPE=BAR}  → (ref: FOO)     data reference
 *   table                      → [N rows] — use 'x NAME' to expand
 *   long string                → truncated to 100 chars with …
 *
 * @param {Array} variables - [{ name, type, value, metaType, tableLines }]
 */
function printVarList(variables) {
  if (variables.length === 0) {
    console.log('\n  No variables at current position.');
    return;
  }

  // Build display rows first so we can measure column widths.
  const rows = variables.map(({ name, type, value, metaType, tableLines }) => {
    const dispName = (name.startsWith('{') && name.endsWith('}')
      ? name.slice(1, -1)
      : name).toLowerCase();
    const dispType = (type || '').toLowerCase();

    let dispValue;
    if (metaType === 'table') {
      dispValue = `[${tableLines} rows] — use 'x ${dispName}' to expand`;
    } else if (typeof value === 'string' && value.startsWith('{O:INITIAL}')) {
      dispValue = '(null)';
    } else if (typeof value === 'string' && value.startsWith('{O:')) {
      const classMatch = value.match(/\\CLASS=([A-Z0-9_]+)/i);
      dispValue = classMatch ? `(${classMatch[1].toLowerCase()})` : value;
    } else if (typeof value === 'string' && value.startsWith('{A:')) {
      const classMatch = value.match(/\\CLASS=([A-Z0-9_]+)/i);
      dispValue = classMatch ? `(ref: ${classMatch[1].toLowerCase()})` : '(ref)';
    } else {
      const str = String(value || '');
      dispValue = str.length > 100 ? str.slice(0, 100) + '…' : str;
    }

    return { dispName, dispType, dispValue };
  });

  const nameW = Math.min(40, Math.max(4, ...rows.map(r => r.dispName.length)));
  const typeW = Math.min(30, Math.max(4, ...rows.map(r => r.dispType.length)));

  console.log('\n  Variables:\n');
  console.log('  ' + 'Name'.padEnd(nameW + 2) + 'Type'.padEnd(typeW + 2) + 'Value');
  console.log('  ' + '-'.repeat(nameW + typeW + 20));
  rows.forEach(({ dispName, dispType, dispValue }) => {
    const nameCol = dispName.length > nameW
      ? dispName.slice(0, nameW - 1) + '…'
      : dispName.padEnd(nameW + 2);
    console.log('  ' + nameCol + dispType.padEnd(typeW + 2) + dispValue);
  });
  console.log('');
}

module.exports = { printVarList };
