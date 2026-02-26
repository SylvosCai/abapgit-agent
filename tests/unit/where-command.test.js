/**
 * Unit tests for where command
 * Tests where-used list functionality
 */

const verifiers = require('../integration/verify-output-spec');

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

describe('Where Command - Logic Tests', () => {
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

describe('Where Command - CLI Output Format', () => {
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

  test('output matches spec format for where-used list', async () => {
    const whereCommand = require('../../src/commands/where');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: true,
          OBJECTS: [{
            OBJECT: 'ZCL_MY_CLASS',
            TYPE: 'CLAS',
            TYPE_TEXT: 'Class',
            REFERENCE_COUNT: 3,
            REFERENCES: [
              { REF_OBJECT: 'ZCL_OTHER_CLASS', REF_TYPE: 'CLAS', REF_TEXT: 'Class' },
              { REF_OBJECT: 'ZIF_INTERFACE', REF_TYPE: 'INTF', REF_TEXT: 'Interface' },
              { REF_OBJECT: 'ZMY_PROGRAM', REF_TYPE: 'PROG', REF_TEXT: 'Program' }
            ]
          }],
          PAGINATION: {
            LIMIT: 50,
            OFFSET: 0,
            TOTAL: 3,
            HAS_MORE: false
          }
        })
      }))
    };

    await whereCommand.execute(['--objects', 'ZCL_MY_CLASS'], mockContext);

    const output = consoleOutput.join('\n');

    // Check for where-used list output elements
    expect(output).toMatch(/Where-used list/i);
    expect(output).toMatch(/\(CLAS\)/); // Object type is shown
    expect(output).toMatch(/reference/i); // Reference text
  });

  test('output handles no references', async () => {
    const whereCommand = require('../../src/commands/where');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: true,
          OBJECTS: [{
            OBJECT: 'ZCL_UNUSED_CLASS',
            TYPE: 'CLAS',
            TYPE_TEXT: 'Class',
            REFERENCE_COUNT: 0,
            REFERENCES: []
          }],
          PAGINATION: {
            LIMIT: 50,
            OFFSET: 0,
            TOTAL: 0,
            HAS_MORE: false
          }
        })
      }))
    };

    await whereCommand.execute(['--objects', 'ZCL_UNUSED_CLASS'], mockContext);

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/No references|0 reference/i);
    expect(output).toMatch(/\(CLAS\)/); // Object type is shown
  });

  test('output shows pagination when has more results', async () => {
    const whereCommand = require('../../src/commands/where');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: true,
          OBJECTS: [{
            OBJECT: 'ZCL_POPULAR_CLASS',
            TYPE: 'CLAS',
            TYPE_TEXT: 'Class',
            REFERENCE_COUNT: 100,
            REFERENCES: Array(50).fill({
              REF_OBJECT: 'ZCL_REF',
              REF_TYPE: 'CLAS',
              REF_TEXT: 'Class'
            })
          }],
          PAGINATION: {
            LIMIT: 50,
            OFFSET: 0,
            TOTAL: 100,
            HAS_MORE: true,
            NEXT_OFFSET: 50
          }
        })
      }))
    };

    await whereCommand.execute(['--objects', 'ZCL_POPULAR_CLASS', '--limit', '50'], mockContext);

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/50.*100/); // Shows X of Y
    expect(output).toMatch(/--offset 50/i); // Next offset suggestion
  });
});
