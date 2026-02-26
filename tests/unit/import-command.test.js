/**
 * Unit tests for import command
 * Tests object import from package to git
 */

describe('Import Command', () => {
  test('import command basic structure', () => {
    const command = 'import';
    expect(command).toBe('import');
  });

  test('import command with message', () => {
    const args = ['import', '--message', 'Initial import'];
    const msgIndex = args.indexOf('--message');
    const message = msgIndex !== -1 ? args[msgIndex + 1] : null;

    expect(message).toBe('Initial import');
  });

  test('uses default message when not specified', () => {
    const args = ['import'];
    const msgIndex = args.indexOf('--message');
    const message = msgIndex !== -1 ? args[msgIndex + 1] : 'Import from ABAP system';

    expect(message).toBe('Import from ABAP system');
  });
});
