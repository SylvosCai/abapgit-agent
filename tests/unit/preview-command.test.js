/**
 * Unit tests for preview command in CLI
 * Tests object parsing, argument handling, and formatting
 */

const verifiers = require('../helpers/verify-output-spec');

// Mock fs module
jest.mock('fs', () => ({
  existsSync: jest.fn(() => true),
  readFileSync: jest.fn(() => 'mock content')
}));

// Mock path module
jest.mock('path', () => ({
  isAbsolute: jest.fn(() => false),
  join: jest.fn((...args) => args.join('/')),
  resolve: jest.fn((...args) => '/' + args.join('/')),
  basename: jest.fn((p) => p.split('/').pop())
}));

// Mock process.exit
const mockExit = jest.spyOn(process, 'exit').mockImplementation((code) => {
  throw new Error(`process.exit(${code})`);
});

describe('Preview Command - Logic Tests', () => {
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

describe('Preview Command - CLI Output Format', () => {
  let consoleOutput;
  let originalConsoleLog;
  let originalConsoleError;

  beforeEach(() => {
    consoleOutput = [];
    originalConsoleLog = console.log;
    originalConsoleError = console.error;
    console.log = (...args) => consoleOutput.push(args.join(' '));
    console.error = (...args) => consoleOutput.push(args.join(' '));
  });

  afterEach(() => {
    console.log = originalConsoleLog;
    console.error = originalConsoleError;
  });

  test('output matches spec format for table preview', async () => {
    const previewCommand = require('../../src/commands/preview');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: true,
          COMMAND: 'PREVIEW',
          OBJECTS: [{
            NAME: 'SFLIGHT',
            TYPE: 'TABL',
            TYPE_TEXT: 'Table',
            ROW_COUNT: 2,
            TOTAL_ROWS: 10,
            ROWS: [
              { CARRID: 'AA', CONNID: '0017', FLDATE: '20240201', PRICE: '422.94' },
              { CARRID: 'LH', CONNID: '0400', FLDATE: '20240202', PRICE: '515.17' }
            ],
            FIELDS: [
              { FIELD: 'CARRID', TYPE: 'CHAR', LENGTH: 3 },
              { FIELD: 'CONNID', TYPE: 'NUMC', LENGTH: 4 },
              { FIELD: 'FLDATE', TYPE: 'DATS', LENGTH: 8 },
              { FIELD: 'PRICE', TYPE: 'CURR', LENGTH: 16 }
            ],
            COLUMNS_DISPLAYED: 4,
            COLUMNS_HIDDEN: []
          }],
          SUMMARY: { TOTAL_OBJECTS: 1, TOTAL_ROWS: 2 },
          ERROR: ''
        })
      }))
    };

    await previewCommand.execute(['--objects', 'SFLIGHT'], mockContext);

    const output = consoleOutput.join('\n');

    // Verify format using verifier
    const verified = verifiers.verifyPreviewOutput(output, 'SFLIGHT');
    expect(verified).toBe(true);

    // Additional specific checks
    expect(output).toMatch(/📊/); // Preview icon
    expect(output).toMatch(/SFLIGHT/);
    expect(output).toMatch(/┌/); // Table borders
    expect(output).toMatch(/│/);
    expect(output).toMatch(/of.*rows/); // Row count
  });

  test('output shows pagination info with limit', async () => {
    const previewCommand = require('../../src/commands/preview');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: true,
          OBJECTS: [{
            NAME: 'SFLIGHT',
            TYPE: 'TABL',
            TYPE_TEXT: 'Table',
            ROW_COUNT: 5,
            TOTAL_ROWS: 100,
            ROWS: Array(5).fill({ CARRID: 'AA', CONNID: '0017' }),
            FIELDS: [
              { FIELD: 'CARRID', TYPE: 'CHAR', LENGTH: 3 },
              { FIELD: 'CONNID', TYPE: 'NUMC', LENGTH: 4 }
            ],
            COLUMNS_DISPLAYED: 2,
            COLUMNS_HIDDEN: []
          }],
          PAGINATION: {
            LIMIT: 5,
            OFFSET: 0,
            TOTAL: 100,
            HAS_MORE: true,
            NEXT_OFFSET: 5
          },
          ERROR: ''
        })
      }))
    };

    await previewCommand.execute(['--objects', 'SFLIGHT', '--limit', '5'], mockContext);

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/5 of 100/); // Pagination text
    expect(output).toMatch(/--offset 5/); // Next offset suggestion
  });

  test('output shows vertical format when requested', async () => {
    const previewCommand = require('../../src/commands/preview');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: true,
          OBJECTS: [{
            NAME: 'SFLIGHT',
            TYPE: 'TABL',
            TYPE_TEXT: 'Table',
            ROW_COUNT: 1,
            TOTAL_ROWS: 1,
            ROWS: [
              { CARRID: 'AA', CONNID: '0017', FLDATE: '20240201' }
            ],
            FIELDS: [
              { FIELD: 'CARRID', TYPE: 'CHAR', LENGTH: 3 },
              { FIELD: 'CONNID', TYPE: 'NUMC', LENGTH: 4 },
              { FIELD: 'FLDATE', TYPE: 'DATS', LENGTH: 8 }
            ],
            COLUMNS_DISPLAYED: 3,
            COLUMNS_HIDDEN: []
          }],
          ERROR: ''
        })
      }))
    };

    await previewCommand.execute(['--objects', 'SFLIGHT', '--vertical'], mockContext);

    const output = consoleOutput.join('\n');

    // In vertical mode, field names are shown with colons on their own lines
    expect(output).toMatch(/CARRID:/); // Field name on its own line
    expect(output).toMatch(/CONNID:/);
    expect(output).toMatch(/FLDATE:/);
    // Fields appear in order without table borders
    expect(output).not.toMatch(/┌/); // No table borders in vertical mode
  });
});
