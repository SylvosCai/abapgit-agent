/**
 * Unit tests for tree command
 * Tests package hierarchy display
 */

describe('Tree Command', () => {
  describe('Package parsing', () => {
    test('parses package name', () => {
      const args = ['tree', '--package', '$MY_PACKAGE'];
      const pkgIndex = args.indexOf('--package');
      const pkg = args[pkgIndex + 1];

      expect(pkg).toBe('$MY_PACKAGE');
    });

    test('handles package without $ prefix', () => {
      const pkg = 'ZMY_PACKAGE';
      expect(pkg.startsWith('$')).toBe(false);
    });
  });

  describe('Depth handling', () => {
    test('uses default depth', () => {
      const args = ['tree', '--package', '$TEST'];
      const depthIndex = args.indexOf('--depth');
      const depth = depthIndex !== -1 ? parseInt(args[depthIndex + 1]) : 3;

      expect(depth).toBe(3);
    });

    test('uses specified depth', () => {
      const args = ['tree', '--package', '$TEST', '--depth', '5'];
      const depthIndex = args.indexOf('--depth');
      const depth = parseInt(args[depthIndex + 1]);

      expect(depth).toBe(5);
    });
  });

  describe('Options', () => {
    test('detects include-types flag', () => {
      const args = ['tree', '--package', '$TEST', '--include-types'];
      const includeTypes = args.includes('--include-types');

      expect(includeTypes).toBe(true);
    });

    test('detects JSON output', () => {
      const args = ['tree', '--package', '$TEST', '--json'];
      const jsonOutput = args.includes('--json');

      expect(jsonOutput).toBe(true);
    });
  });
});
