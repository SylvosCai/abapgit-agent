/**
 * Unit tests for tree command
 * Tests package hierarchy display
 */

const verifiers = require('../helpers/output-verifiers');

// Mock fs module
jest.mock('fs', () => ({
  existsSync: jest.fn(() => true),
  readFileSync: jest.fn((p) => p.includes('.abapgit-agent.json') ? '{}' : 'mock content')
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

describe('Tree Command - Logic Tests', () => {
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

describe('Tree Command - CLI Output Format', () => {
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

  test.each([
    [[], 3, false],
    [['--depth', '2', '--include-types'], 2, true],
    [['--depth', '5', '--include-objects'], 5, true]
  ])('JSON execution preserves the response and forwards options %j', async (options, depth, includeTypes) => {
    const result = {
      SUCCESS: true,
      PACKAGE: '$ZMAIN',
      NODES: [{ PACKAGE: '$ZMAIN', DEPTH: 0 }],
      TOTAL_PACKAGES: 1,
      TOTAL_OBJECTS: 0,
      MESSAGE: 'Tree retrieved\nwithout formatting changes'
    };
    const config = { host: 'test', port: 443 };
    const http = {
      fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
      post: jest.fn().mockResolvedValue(result)
    };
    const context = {
      loadConfig: jest.fn(() => config),
      AbapHttp: jest.fn(() => http)
    };

    await require('../../src/commands/tree').execute(['--package', '$ZMAIN', '--json', ...options], context);

    expect(context.loadConfig).toHaveBeenCalledTimes(1);
    expect(context.AbapHttp).toHaveBeenCalledWith(config);
    expect(context.AbapHttp).toHaveBeenCalledTimes(1);
    expect(http.fetchCsrfToken).toHaveBeenCalledTimes(1);
    expect(http.post).toHaveBeenCalledTimes(1);
    expect(http.post).toHaveBeenCalledWith('/sap/bc/z_abapgit_agent/tree', {
      package: '$ZMAIN', depth, include_objects: includeTypes
    }, { csrfToken: 'token123' });
    expect(consoleOutput).toHaveLength(1);
    expect(JSON.parse(consoleOutput[0])).toEqual(result);
  });

  test.each(['fetchCsrfToken', 'post'])('JSON execution propagates %s failures without output', async (method) => {
    const error = new Error('HTTP request failed');
    const http = {
      fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
      post: jest.fn().mockResolvedValue({ SUCCESS: true })
    };
    http[method].mockRejectedValue(error);
    const context = { loadConfig: jest.fn(() => ({})), AbapHttp: jest.fn(() => http) };

    await expect(require('../../src/commands/tree').execute(['--package', '$ZMAIN', '--json'], context))
      .rejects.toBe(error);
    expect(http.fetchCsrfToken).toHaveBeenCalledTimes(1);
    expect(http.post).toHaveBeenCalledTimes(method === 'post' ? 1 : 0);
    expect(consoleOutput).toEqual([]);
  });

  test('output matches spec format for package tree', async () => {
    const treeCommand = require('../../src/commands/tree');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: true,
          COMMAND: 'TREE',
          PACKAGE: '$ZMAIN',
          PARENT_PACKAGE: '$ZBASE',
          NODES: [
            { PACKAGE: '$ZMAIN', PARENT: '', DEPTH: 0, OBJECT_COUNT: 10 },
            { PACKAGE: '$ZMAIN_SUB1', PARENT: '$ZMAIN', DEPTH: 1, OBJECT_COUNT: 5 },
            { PACKAGE: '$ZMAIN_SUB2', PARENT: '$ZMAIN', DEPTH: 1, OBJECT_COUNT: 3 }
          ],
          TOTAL_PACKAGES: 3,
          TOTAL_OBJECTS: 18,
          OBJECTS: [],
          ERROR: ''
        })
      }))
    };

    await treeCommand.execute(['--package', '$ZMAIN'], mockContext);

    const output = consoleOutput.join('\n');

    // Verify format using verifier
    const verified = verifiers.verifyTreeOutput(output, '$ZMAIN');
    expect(verified).toBe(true);

    // Additional specific checks
    expect(output).toMatch(/Package Tree: \$ZMAIN/);
    expect(output).toMatch(/📦/); // Tree icons
    expect(output).toMatch(/├─|└─/); // Tree branches
    expect(output).toMatch(/Summary/);
    expect(output).toMatch(/PACKAGES: 3/);
    expect(output).toMatch(/OBJECTS: 18/);
  });

  test('output shows parent package when available', async () => {
    const treeCommand = require('../../src/commands/tree');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: true,
          PACKAGE: '$ZMAIN',
          PARENT_PACKAGE: '$ZPARENT',
          NODES: [
            { PACKAGE: '$ZMAIN', PARENT: '', DEPTH: 0, OBJECT_COUNT: 10 }
          ],
          TOTAL_PACKAGES: 1,
          TOTAL_OBJECTS: 10,
          OBJECTS: []
        })
      }))
    };

    await treeCommand.execute(['--package', '$ZMAIN'], mockContext);

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/Parent: \$ZPARENT/);
    expect(output).toMatch(/⬆️/); // Parent arrow emoji
  });

  test('output includes object types when --include-types specified', async () => {
    const treeCommand = require('../../src/commands/tree');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: true,
          PACKAGE: '$ZMAIN',
          NODES: [
            { PACKAGE: '$ZMAIN', PARENT: '', DEPTH: 0, OBJECT_COUNT: 10 }
          ],
          TOTAL_PACKAGES: 1,
          TOTAL_OBJECTS: 10,
          OBJECTS: [
            { OBJECT: 'CLAS', COUNT: 5 },
            { OBJECT: 'INTF', COUNT: 3 },
            { OBJECT: 'TABL', COUNT: 2 }
          ]
        })
      }))
    };

    await treeCommand.execute(['--package', '$ZMAIN', '--include-types'], mockContext);

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/TYPES:/);
    expect(output).toMatch(/CLAS=5/);
    expect(output).toMatch(/INTF=3/);
    expect(output).toMatch(/TABL=2/);
  });
});
