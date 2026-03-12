/**
 * Unit tests for view command in CLI
 * Tests object parsing and request building
 */

const verifiers = require('../helpers/output-verifiers');

// Mock fs module
// existsSync returns false by default so local file lookup silently fails,
// keeping unit tests self-contained (no actual file system or ADT calls).
jest.mock('fs', () => ({
  existsSync: jest.fn(() => false),
  readFileSync: jest.fn(() => { throw new Error('no file'); })
}));

// Mock path module
jest.mock('path', () => ({
  isAbsolute: jest.fn(() => false),
  join: jest.fn((...args) => args.join('/')),
  resolve: jest.fn((...args) => '/' + args.join('/')),
  basename: jest.fn((p) => p.split('/').pop()),
  dirname: jest.fn((p) => p.split('/').slice(0, -1).join('/'))
}));

// Mock process.exit
const mockExit = jest.spyOn(process, 'exit').mockImplementation((code) => {
  throw new Error(`process.exit(${code})`);
});

describe('View Command - Logic Tests', () => {
  describe('Object parsing', () => {
    test('parses single object name', () => {
      const objectsArg = 'ZCL_MY_CLASS';
      const objects = objectsArg.split(',').map(o => o.trim());

      expect(objects.length).toBe(1);
      expect(objects[0]).toBe('ZCL_MY_CLASS');
    });

    test('parses multiple object names', () => {
      const objectsArg = 'ZCL_CLASS1,ZCL_CLASS2,ZIF_INTERFACE';
      const objects = objectsArg.split(',').map(o => o.trim());

      expect(objects.length).toBe(3);
      expect(objects).toContain('ZCL_CLASS1');
      expect(objects).toContain('ZIF_INTERFACE');
    });

    test('handles spaces after commas', () => {
      const objectsArg = 'ZCL_CLASS1, ZCL_CLASS2';
      const objects = objectsArg.split(',').map(o => o.trim());

      expect(objects[1]).toBe('ZCL_CLASS2');
    });
  });

  describe('Type detection', () => {
    test('auto-detects type from TADIR when not specified', () => {
      const object = 'ZCL_MY_CLASS';
      const explicitType = null; // Not specified
      const shouldAutoDetect = explicitType === null;

      expect(shouldAutoDetect).toBe(true);
    });

    test('uses explicit type when specified', () => {
      const args = ['view', '--objects', 'ZCL_TEST', '--type', 'CLAS'];
      const typeIndex = args.indexOf('--type');
      const type = typeIndex !== -1 ? args[typeIndex + 1] : null;

      expect(type).toBe('CLAS');
    });

    test('handles various object types', () => {
      const types = ['CLAS', 'INTF', 'TABL', 'STRU', 'DTEL', 'TTYP', 'DDLS'];

      types.forEach(type => {
        expect(type).toMatch(/^[A-Z]{4}$/);
      });
    });
  });

  describe('Response handling', () => {
    test('handles successful response for class', () => {
      const response = {
        success: true,
        objects: [{
          name: 'ZCL_MY_CLASS',
          type: 'CLAS',
          type_text: 'Class',
          source: 'CLASS zcl_my_class DEFINITION...',
          not_found: false
        }]
      };

      const success = response.success || response.SUCCESS;
      const objects = response.objects || response.OBJECTS || [];

      expect(success).toBe(true);
      expect(objects[0].type).toBe('CLAS');
    });

    test('handles object not found', () => {
      const response = {
        success: false,
        objects: [{
          name: 'ZCL_NONEXISTENT',
          not_found: true
        }]
      };

      const objects = response.objects || response.OBJECTS || [];

      expect(objects[0].not_found).toBe(true);
    });

    test('handles multiple objects in response', () => {
      const response = {
        success: true,
        objects: [
          { name: 'ZCL_CLASS1', type: 'CLAS' },
          { name: 'ZIF_INTERFACE', type: 'INTF' },
          { name: 'ZTABLE', type: 'TABL' }
        ]
      };

      const objects = response.objects || response.OBJECTS || [];

      expect(objects.length).toBe(3);
      expect(objects.map(o => o.type)).toEqual(['CLAS', 'INTF', 'TABL']);
    });
  });

  describe('JSON output mode', () => {
    test('detects JSON output flag', () => {
      const args = ['view', '--objects', 'ZCL_TEST', '--json'];
      const jsonOutput = args.includes('--json');

      expect(jsonOutput).toBe(true);
    });

    test('JSON flag not present by default', () => {
      const args = ['view', '--objects', 'ZCL_TEST'];
      const jsonOutput = args.includes('--json');

      expect(jsonOutput).toBe(false);
    });
  });

  describe('Full mode flag', () => {
    test('detects --full flag', () => {
      const args = ['view', '--objects', 'ZCL_TEST', '--full'];
      expect(args.includes('--full')).toBe(true);
    });

    test('--full flag not present by default', () => {
      const args = ['view', '--objects', 'ZCL_TEST'];
      expect(args.includes('--full')).toBe(false);
    });

    test('builds request with full: true when --full present', () => {
      const args = ['view', '--objects', 'ZCL_TEST', '--full'];
      const fullMode = args.includes('--full');
      const data = { objects: ['ZCL_TEST'] };
      if (fullMode) data.full = true;
      expect(data.full).toBe(true);
    });

    test('does not include full in request without --full', () => {
      const args = ['view', '--objects', 'ZCL_TEST'];
      const fullMode = args.includes('--full');
      const data = { objects: ['ZCL_TEST'] };
      if (fullMode) data.full = true;
      expect(data.full).toBeUndefined();
    });
  });

  describe('Request building', () => {
    test('builds request for single object', () => {
      const request = {
        objects: ['ZCL_MY_CLASS'],
        type: null // Auto-detect
      };

      expect(request.objects.length).toBe(1);
    });

    test('builds request for multiple objects', () => {
      const request = {
        objects: ['ZCL_CLASS1', 'ZCL_CLASS2'],
        type: null
      };

      expect(request.objects.length).toBe(2);
    });

    test('includes type when specified', () => {
      const request = {
        objects: ['ZMY_TABLE'],
        type: 'TABL'
      };

      expect(request.type).toBe('TABL');
    });
  });

  describe('Special object types', () => {
    test('handles table type (TTYP)', () => {
      const object = 'ZMY_TABLE_TYPE';
      const type = 'TTYP';

      expect(type).toBe('TTYP');
    });

    test('handles structure (STRU)', () => {
      const object = 'ZMY_STRUCTURE';
      const type = 'STRU';

      expect(type).toBe('STRU');
    });

    test('handles data element (DTEL)', () => {
      const object = 'ZMY_DATA_ELEMENT';
      const type = 'DTEL';

      expect(type).toBe('DTEL');
    });

    test('handles CDS view (DDLS)', () => {
      const object = 'ZC_MY_CDS_VIEW';
      const type = 'DDLS';

      expect(type).toBe('DDLS');
    });
  });
});

describe('View Command - CLI Output Format', () => {
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

  test('output matches spec format for class view', async () => {
    const viewCommand = require('../../src/commands/view');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          success: true,
          command: 'VIEW',
          message: 'Retrieved object(s)',
          objects: [{
            name: 'ZCL_MY_CLASS',
            type: 'CLAS',
            type_text: 'Class',
            description: 'Class ZCL_MY_CLASS in $PACKAGE',
            source: 'CLASS zcl_my_class DEFINITION PUBLIC.\n  PUBLIC SECTION.\n    METHODS: constructor.\nENDCLASS.',
            not_found: false
          }],
          summary: { total: 1 }
        })
      }))
    };

    await viewCommand.execute(['--objects', 'ZCL_MY_CLASS'], mockContext);

    const output = consoleOutput.join('\n');

    // Verify format using verifier
    const verified = verifiers.verifyViewOutput(output, 'ZCL_MY_CLASS');
    expect(verified).toBe(true);

    // Additional specific checks
    expect(output).toMatch(/📖/); // View icon
    expect(output).toMatch(/ZCL_MY_CLASS/);
    expect(output).toMatch(/\(Class\)/);
    expect(output).toMatch(/CLASS zcl_my_class/); // Source code
  });

  test('output handles multiple objects', async () => {
    const viewCommand = require('../../src/commands/view');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          success: true,
          objects: [
            {
              name: 'ZCL_CLASS1',
              type: 'CLAS',
              type_text: 'Class',
              description: 'Class ZCL_CLASS1',
              source: 'CLASS zcl_class1 DEFINITION...',
              not_found: false
            },
            {
              name: 'ZIF_INTERFACE1',
              type: 'INTF',
              type_text: 'Interface',
              description: 'Interface ZIF_INTERFACE1',
              source: 'INTERFACE zif_interface1 PUBLIC...',
              not_found: false
            }
          ],
          summary: { total: 2 }
        })
      }))
    };

    await viewCommand.execute(['--objects', 'ZCL_CLASS1,ZIF_INTERFACE1'], mockContext);

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/ZCL_CLASS1/);
    expect(output).toMatch(/ZIF_INTERFACE1/);
    expect(output).toMatch(/\(Class\)/);
    expect(output).toMatch(/\(Interface\)/);
  });

  test('output handles object not found', async () => {
    const viewCommand = require('../../src/commands/view');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          success: false,
          objects: [{
            name: 'ZCL_NONEXISTENT',
            not_found: true
          }],
          summary: { total: 1 }
        })
      }))
    };

    await viewCommand.execute(['--objects', 'ZCL_NONEXISTENT'], mockContext);

    const output = consoleOutput.join('\n');

    // When success=false, shows error message
    expect(output).toMatch(/Error:|Failed/i);
  });

  test('--full output renders dual line numbers (global + include-relative) for CM sections', async () => {
    const viewCommand = require('../../src/commands/view');
    const fs = require('fs');

    // Provide a mock local .clas.abap file whose line numbers drive global_start.
    // METHOD constructor is at line 7, METHOD execute is at line 11.
    const mockClassSource = [
      'CLASS zcl_my_class DEFINITION.',  // 1
      '  PUBLIC SECTION.',               // 2
      'ENDCLASS.',                       // 3
      '',                                // 4
      'CLASS zcl_my_class IMPLEMENTATION.', // 5
      '',                                // 6
      '  METHOD constructor.',           // 7  ← CM001 global_start
      '  mv_x = 1.',                     // 8
      'ENDMETHOD.',                      // 9
      '',                                // 10
      '  METHOD execute.',               // 11  ← CM002 global_start
      '  RETURN.',                       // 12
      'ENDMETHOD.',                      // 13
    ].join('\n');

    fs.existsSync.mockImplementation((p) => p.includes('zcl_my_class.clas.abap'));
    fs.readFileSync.mockImplementation((p) => {
      if (p.includes('zcl_my_class.clas.abap')) return mockClassSource;
      if (p.includes('.abapGitAgent')) throw new Error('no config');
      throw new Error('no file');
    });

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          success: true,
          message: 'Retrieved object(s)',
          objects: [{
            name: 'ZCL_MY_CLASS',
            type: 'CLAS',
            type_text: 'Class',
            description: 'Class ZCL_MY_CLASS in $PACKAGE',
            not_found: false,
            sections: [
              { suffix: 'CU', description: 'Public Section', lines: ['CLASS zcl_my_class DEFINITION.', '  PUBLIC SECTION.', 'ENDCLASS.'] },
              { suffix: 'CM001', description: 'Class Method', method_name: 'CONSTRUCTOR', lines: ['METHOD constructor.', '  mv_x = 1.', 'ENDMETHOD.'] },
              { suffix: 'CM002', description: 'Class Method', method_name: 'EXECUTE', lines: ['METHOD execute.', '  RETURN.', 'ENDMETHOD.'] }
            ]
          }],
          summary: { total: 1 }
        })
      }))
    };

    await viewCommand.execute(['--objects', 'ZCL_MY_CLASS', '--full'], mockContext);

    const output = consoleOutput.join('\n');

    // Method header contains CM suffix and ready-to-use debug set --objects hint with global_start line
    expect(output).toMatch(/CONSTRUCTOR.*CM001.*debug set.*ZCL_MY_CLASS:7/);
    expect(output).toMatch(/EXECUTE.*CM002.*debug set.*ZCL_MY_CLASS:11/);

    // CM001 lines: global G [N] dual format — METHOD at 7, so global lines 7,8,9
    expect(output).toMatch(/^\s+7\s+\[\s*1\]\s+METHOD constructor/m);
    expect(output).toMatch(/^\s+9\s+\[\s*3\]\s+ENDMETHOD/m);

    // CM002 include-relative restarts at 1, global starts at 11
    expect(output).toMatch(/^\s+11\s+\[\s*1\]\s+METHOD execute/m);

    // No "global line N" text in method headers
    expect(output).not.toMatch(/global line \d+/);

    // Reset mocks to defaults after this test
    fs.existsSync.mockImplementation(() => false);
    fs.readFileSync.mockImplementation(() => { throw new Error('no file'); });
  });

  test('--full output sends full: true in request', async () => {
    const viewCommand = require('../../src/commands/view');
    let capturedData;

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockImplementation((url, data) => {
          capturedData = data;
          return Promise.resolve({
            success: true,
            message: 'Retrieved object(s)',
            objects: [{ name: 'ZCL_TEST', type: 'CLAS', type_text: 'Class', not_found: false, sections: [] }],
            summary: { total: 1 }
          });
        })
      }))
    };

    await viewCommand.execute(['--objects', 'ZCL_TEST', '--full'], mockContext);

    expect(capturedData.full).toBe(true);
  });

  test('request without --full does not send full field', async () => {
    const viewCommand = require('../../src/commands/view');
    let capturedData;

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockImplementation((url, data) => {
          capturedData = data;
          return Promise.resolve({
            success: true,
            message: 'Retrieved object(s)',
            objects: [{ name: 'ZCL_TEST', type: 'CLAS', type_text: 'Class', source: 'CLASS zcl_test DEFINITION.', not_found: false }],
            summary: { total: 1 }
          });
        })
      }))
    };

    await viewCommand.execute(['--objects', 'ZCL_TEST'], mockContext);

    expect(capturedData.full).toBeUndefined();
  });

  test('non-CM sections in --full output use section-local line numbers only', async () => {
    const viewCommand = require('../../src/commands/view');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          success: true,
          message: 'Retrieved object(s)',
          objects: [{
            name: 'ZIF_MY_INTF',
            type: 'INTF',
            type_text: 'Interface',
            description: 'Interface',
            not_found: false,
            sections: [
              { suffix: 'IU', description: 'Interface Section', lines: ['INTERFACE zif_my_intf PUBLIC.', '  METHODS do_it.', 'ENDINTERFACE.'] }
            ]
          }],
          summary: { total: 1 }
        })
      }))
    };

    await viewCommand.execute(['--objects', 'ZIF_MY_INTF', '--full'], mockContext);

    const output = consoleOutput.join('\n');

    // Section-local line numbers present (no local file → globalStart=0 → falls back to include-relative)
    expect(output).toMatch(/^\s+1\s+INTERFACE/m);
    // No include-relative brackets (non-CM section)
    expect(output).not.toMatch(/\[\s*\d+\]/);
  });
});
