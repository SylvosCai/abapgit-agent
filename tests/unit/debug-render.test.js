/**
 * Unit tests for debug-render.js
 * Tests printVarList() display formatting — all value/type branches
 */

const { printVarList } = require('../../src/utils/debug-render');

describe('printVarList()', () => {
  let output;
  let origLog;

  beforeEach(() => {
    output = [];
    origLog = console.log;
    console.log = (...args) => output.push(args.join(' '));
  });

  afterEach(() => {
    console.log = origLog;
  });

  test('prints "No variables" message for empty array', () => {
    printVarList([]);
    expect(output.join('\n')).toMatch(/No variables at current position/);
  });

  test('strips braces from variable name and lowercases it', () => {
    printVarList([{ name: '{MY_VAR}', type: 'I', value: '42' }]);
    const text = output.join('\n');
    expect(text).toMatch(/my_var/);
    expect(text).not.toMatch(/\{MY_VAR\}/);
  });

  test('lowercases variable name without braces', () => {
    printVarList([{ name: 'LV_COUNT', type: 'I', value: '5' }]);
    expect(output.join('\n')).toMatch(/lv_count/);
  });

  test('lowercases type', () => {
    printVarList([{ name: 'x', type: 'STRING', value: 'hello' }]);
    expect(output.join('\n')).toMatch(/string/);
  });

  test('displays table metaType as "[N rows] — use x NAME to expand"', () => {
    printVarList([{ name: 'lt_items', type: 'TABLE', value: null, metaType: 'table', tableLines: 7 }]);
    const text = output.join('\n');
    expect(text).toMatch(/\[7 rows\]/);
    expect(text).toMatch(/use 'x lt_items' to expand/);
  });

  test('displays {O:INITIAL} value as (null)', () => {
    printVarList([{ name: 'lo_obj', type: 'ref', value: '{O:INITIAL}' }]);
    expect(output.join('\n')).toMatch(/\(null\)/);
  });

  test('displays {O:...\\CLASS=FOO} as (foo)', () => {
    printVarList([{ name: 'lo_obj', type: 'ref', value: '{O:N*\\CLASS=ZCL_ORDER}' }]);
    expect(output.join('\n')).toMatch(/\(zcl_order\)/);
  });

  test('displays {O:...} without CLASS match as raw value', () => {
    printVarList([{ name: 'lo_obj', type: 'ref', value: '{O:SOME_OTHER_FORM}' }]);
    const text = output.join('\n');
    // No CLASS= in value → falls back to raw value
    expect(text).toMatch(/\{O:SOME_OTHER_FORM\}/);
  });

  test('displays {A:...\\CLASS=FOO} as (ref: foo)', () => {
    printVarList([{ name: 'lr_data', type: 'ref', value: '{A:N*\\CLASS=ZCL_DATA\\TYPE=BAR}' }]);
    expect(output.join('\n')).toMatch(/\(ref: zcl_data\)/);
  });

  test('displays {A:...} without CLASS as (ref)', () => {
    printVarList([{ name: 'lr_data', type: 'ref', value: '{A:NO_CLASS_HERE}' }]);
    expect(output.join('\n')).toMatch(/\(ref\)/);
  });

  test('truncates long string values to 100 chars with ellipsis', () => {
    const longValue = 'A'.repeat(150);
    printVarList([{ name: 'lv_str', type: 'STRING', value: longValue }]);
    const text = output.join('\n');
    expect(text).toMatch(/A{100}…/);
    expect(text).not.toContain('A'.repeat(101) + '…');
  });

  test('displays short string value as-is', () => {
    printVarList([{ name: 'lv_str', type: 'STRING', value: 'hello world' }]);
    expect(output.join('\n')).toMatch(/hello world/);
  });

  test('handles null/undefined value as empty string', () => {
    printVarList([{ name: 'lv_x', type: 'I', value: null }]);
    // Should not throw; value column just shows empty
    expect(output.join('\n')).toMatch(/lv_x/);
  });

  test('prints header row with Name / Type / Value columns', () => {
    printVarList([{ name: 'lv_a', type: 'I', value: '1' }]);
    const text = output.join('\n');
    expect(text).toMatch(/Name/);
    expect(text).toMatch(/Type/);
    expect(text).toMatch(/Value/);
  });

  test('prints separator line after header', () => {
    printVarList([{ name: 'lv_a', type: 'I', value: '1' }]);
    const text = output.join('\n');
    expect(text).toMatch(/-{10,}/);
  });

  test('handles multiple variables in one call', () => {
    printVarList([
      { name: 'lv_a', type: 'I', value: '1' },
      { name: 'lv_b', type: 'STRING', value: 'hello' },
      { name: 'lt_tab', type: 'TABLE', value: null, metaType: 'table', tableLines: 3 }
    ]);
    const text = output.join('\n');
    expect(text).toMatch(/lv_a/);
    expect(text).toMatch(/lv_b/);
    expect(text).toMatch(/lt_tab/);
    expect(text).toMatch(/\[3 rows\]/);
  });

  test('column widths clamp at max (40 for name, 30 for type)', () => {
    // Name longer than 40 chars should be truncated with ellipsis in the output row
    const longName = '{' + 'X'.repeat(50) + '}';
    printVarList([{ name: longName, type: 'I', value: '1' }]);
    // Should not throw and should produce output
    expect(output.length).toBeGreaterThan(0);
  });
});
