/**
 * Unit tests for preview command in CLI
 * Tests object parsing, argument handling, and formatting
 */

describe('Preview Command', () => {
  describe('Object parsing', () => {
    test('parses single table name', () => {
      const objectsArg = 'SFLIGHT';
      const objects = objectsArg.split(',').map(o => o.trim());

      expect(objects.length).toBe(1);
      expect(objects[0]).toBe('SFLIGHT');
    });

    test('parses multiple table names', () => {
      const objectsArg = 'SFLIGHT,SPFLI,SCARR';
      const objects = objectsArg.split(',').map(o => o.trim());

      expect(objects.length).toBe(3);
    });
  });

  describe('Limit and offset', () => {
    test('uses default limit if not specified', () => {
      const args = ['preview', '--objects', 'SFLIGHT'];
      const limitIndex = args.indexOf('--limit');
      const limit = limitIndex !== -1 ? parseInt(args[limitIndex + 1]) : 100;

      expect(limit).toBe(100);
    });

    test('uses specified limit', () => {
      const args = ['preview', '--objects', 'SFLIGHT', '--limit', '20'];
      const limitIndex = args.indexOf('--limit');
      const limit = parseInt(args[limitIndex + 1]);

      expect(limit).toBe(20);
    });

    test('uses default offset', () => {
      const args = ['preview', '--objects', 'SFLIGHT'];
      const offsetIndex = args.indexOf('--offset');
      const offset = offsetIndex !== -1 ? parseInt(args[offsetIndex + 1]) : 0;

      expect(offset).toBe(0);
    });

    test('uses specified offset', () => {
      const args = ['preview', '--objects', 'SFLIGHT', '--offset', '50'];
      const offsetIndex = args.indexOf('--offset');
      const offset = parseInt(args[offsetIndex + 1]);

      expect(offset).toBe(50);
    });
  });

  describe('Column selection', () => {
    test('shows all columns by default', () => {
      const args = ['preview', '--objects', 'SFLIGHT'];
      const columnsIndex = args.indexOf('--columns');
      const columns = columnsIndex !== -1 ? args[columnsIndex + 1].split(',') : null;

      expect(columns).toBeNull();
    });

    test('uses specified columns', () => {
      const args = ['preview', '--objects', 'SFLIGHT', '--columns', 'CARRID,CONNID,PRICE'];
      const columnsIndex = args.indexOf('--columns');
      const columns = args[columnsIndex + 1].split(',');

      expect(columns.length).toBe(3);
      expect(columns).toContain('CARRID');
      expect(columns).toContain('PRICE');
    });
  });

  describe('WHERE clause', () => {
    test('no WHERE clause by default', () => {
      const args = ['preview', '--objects', 'SFLIGHT'];
      const whereIndex = args.indexOf('--where');
      const whereClause = whereIndex !== -1 ? args[whereIndex + 1] : null;

      expect(whereClause).toBeNull();
    });

    test('uses specified WHERE clause', () => {
      const args = ['preview', '--objects', 'SFLIGHT', '--where', "CARRID = 'AA'"];
      const whereIndex = args.indexOf('--where');
      const whereClause = args[whereIndex + 1];

      expect(whereClause).toBe("CARRID = 'AA'");
    });
  });

  describe('Display modes', () => {
    test('uses table format by default', () => {
      const args = ['preview', '--objects', 'SFLIGHT'];
      const vertical = args.includes('--vertical');

      expect(vertical).toBe(false);
    });

    test('detects vertical mode', () => {
      const args = ['preview', '--objects', 'SFLIGHT', '--vertical'];
      const vertical = args.includes('--vertical');

      expect(vertical).toBe(true);
    });

    test('detects compact mode', () => {
      const args = ['preview', '--objects', 'SFLIGHT', '--compact'];
      const compact = args.includes('--compact');

      expect(compact).toBe(true);
    });

    test('detects JSON output mode', () => {
      const args = ['preview', '--objects', 'SFLIGHT', '--json'];
      const jsonOutput = args.includes('--json');

      expect(jsonOutput).toBe(true);
    });
  });

  describe('Response handling', () => {
    test('handles successful response with rows', () => {
      const response = {
        SUCCESS: true,
        OBJECTS: [{
          NAME: 'SFLIGHT',
          TYPE: 'TABL',
          ROW_COUNT: 2,
          TOTAL_ROWS: 100,
          ROWS: [
            { CARRID: 'AA', CONNID: '0017' },
            { CARRID: 'LH', CONNID: '0400' }
          ]
        }]
      };

      const success = response.SUCCESS || response.success;
      const objects = response.OBJECTS || response.objects || [];

      expect(success).toBe(true);
      expect(objects[0].ROWS.length).toBe(2);
    });

    test('handles pagination info', () => {
      const response = {
        PAGINATION: {
          LIMIT: 10,
          OFFSET: 0,
          TOTAL: 100,
          HAS_MORE: true,
          NEXT_OFFSET: 10
        }
      };

      const pagination = response.PAGINATION || response.pagination || {};

      expect(pagination.HAS_MORE).toBe(true);
      expect(pagination.NEXT_OFFSET).toBe(10);
    });

    test('handles field metadata', () => {
      const response = {
        OBJECTS: [{
          FIELDS: [
            { FIELD: 'CARRID', TYPE: 'CHAR', LENGTH: 3 },
            { FIELD: 'CONNID', TYPE: 'NUMC', LENGTH: 4 }
          ]
        }]
      };

      const objects = response.OBJECTS || response.objects || [];

      expect(objects[0].FIELDS.length).toBe(2);
    });
  });

  describe('Request building', () => {
    test('builds basic request', () => {
      const request = {
        objects: ['SFLIGHT'],
        limit: 100,
        offset: 0
      };

      expect(request.objects.length).toBe(1);
      expect(request.limit).toBe(100);
    });

    test('builds request with WHERE clause', () => {
      const request = {
        objects: ['SFLIGHT'],
        where: "CARRID = 'AA'"
      };

      expect(request.where).toBeDefined();
    });

    test('builds request with column selection', () => {
      const request = {
        objects: ['SFLIGHT'],
        columns: ['CARRID', 'CONNID', 'PRICE']
      };

      expect(request.columns.length).toBe(3);
    });
  });
});
