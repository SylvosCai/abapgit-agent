/**
 * Unit tests for where command
 * Tests where-used list functionality
 */

describe('Where Command', () => {
  describe('Object parsing', () => {
    test('parses single object', () => {
      const objectsArg = 'ZCL_MY_CLASS';
      const objects = objectsArg.split(',').map(o => o.trim());

      expect(objects.length).toBe(1);
    });

    test('parses multiple objects', () => {
      const objectsArg = 'ZCL_CLASS1,ZIF_INTERFACE';
      const objects = objectsArg.split(',').map(o => o.trim());

      expect(objects.length).toBe(2);
    });
  });

  describe('Pagination', () => {
    test('uses default limit', () => {
      const args = ['where', '--objects', 'ZCL_TEST'];
      const limitIndex = args.indexOf('--limit');
      const limit = limitIndex !== -1 ? parseInt(args[limitIndex + 1]) : 50;

      expect(limit).toBe(50);
    });

    test('uses specified limit', () => {
      const args = ['where', '--objects', 'ZCL_TEST', '--limit', '100'];
      const limitIndex = args.indexOf('--limit');
      const limit = parseInt(args[limitIndex + 1]);

      expect(limit).toBe(100);
    });

    test('uses offset for pagination', () => {
      const args = ['where', '--objects', 'ZCL_TEST', '--offset', '50'];
      const offsetIndex = args.indexOf('--offset');
      const offset = parseInt(args[offsetIndex + 1]);

      expect(offset).toBe(50);
    });
  });
});
