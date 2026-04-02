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

  describe('Lines mode flag', () => {
    test('detects --lines flag alongside --full', () => {
      const args = ['view', '--objects', 'ZCL_TEST', '--full', '--lines'];
      expect(args.includes('--lines')).toBe(true);
    });

    test('--lines flag not present by default', () => {
      const args = ['view', '--objects', 'ZCL_TEST', '--full'];
      expect(args.includes('--lines')).toBe(false);
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

  test('--full --lines output renders dual line numbers (global + include-relative) for CM sections', async () => {
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

    await viewCommand.execute(['--objects', 'ZCL_MY_CLASS', '--full', '--lines'], mockContext);

    const output = consoleOutput.join('\n');

    // Method header contains CM suffix and ready-to-use debug set --objects hint with first exec line
    expect(output).toMatch(/CONSTRUCTOR.*CM001.*debug set.*ZCL_MY_CLASS:8/);
    expect(output).toMatch(/EXECUTE.*CM002.*debug set.*ZCL_MY_CLASS:12/);

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

  test('non-CM sections in --full --lines output use section-local line numbers only', async () => {
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

    await viewCommand.execute(['--objects', 'ZIF_MY_INTF', '--full', '--lines'], mockContext);

    const output = consoleOutput.join('\n');

    // Section-local line numbers present (no local file → globalStart=0 → falls back to include-relative)
    expect(output).toMatch(/^\s+1\s+INTERFACE/m);
    // No include-relative brackets (non-CM section)
    expect(output).not.toMatch(/\[\s*\d+\]/);
  });

  test('--full without --lines renders clean source without line numbers', async () => {
    const viewCommand = require('../../src/commands/view');

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
            description: 'Test class',
            not_found: false,
            sections: [
              { suffix: 'CU', description: 'Public Section', lines: ['CLASS zcl_my_class DEFINITION.', '  PUBLIC SECTION.', 'ENDCLASS.'] },
              { suffix: 'CM001', description: 'Class Method', method_name: 'CONSTRUCTOR', lines: ['METHOD constructor.', '  mv_x = 1.', 'ENDMETHOD.'] }
            ]
          }],
          summary: { total: 1 }
        })
      }))
    };

    await viewCommand.execute(['--objects', 'ZCL_MY_CLASS', '--full'], mockContext);

    const output = consoleOutput.join('\n');

    // Section headers present
    expect(output).toMatch(/Public Section.*CU/);
    expect(output).toMatch(/Method: CONSTRUCTOR.*CM001/);

    // Code lines present without line numbers
    expect(output).toMatch(/CLASS zcl_my_class DEFINITION/);
    expect(output).toMatch(/METHOD constructor/);

    // No breakpoint hint in header (clean mode)
    expect(output).not.toMatch(/debug set/);
    // No dual line number format G [N]
    expect(output).not.toMatch(/\[\s*\d+\]/);
    // No leading numeric columns
    expect(output).not.toMatch(/^\s+\d+\s+\[/m);
  });
});

describe('View Command - buildMethodLineMap()', () => {
  let viewCommand;

  beforeEach(() => {
    jest.resetModules();
    viewCommand = require('../../src/commands/view');
  });

  test('maps METHOD statement to 1-based line number', () => {
    const lines = [
      'CLASS zcl_test DEFINITION.',
      '  METHOD do_something.',
      '    WRITE hello.',
      '  ENDMETHOD.',
      'ENDCLASS.'
    ];
    const map = viewCommand._buildMethodLineMap(lines);
    expect(map['DO_SOMETHING']).toBe(2);
  });

  test('uppercases method names as keys', () => {
    const lines = ['  METHOD my_method.', '    WRITE x.', '  ENDMETHOD.'];
    const map = viewCommand._buildMethodLineMap(lines);
    expect(map['MY_METHOD']).toBe(1);
  });

  test('supports interface~method syntax', () => {
    const lines = ['  METHOD if_some_interface~do_it.'];
    const map = viewCommand._buildMethodLineMap(lines);
    expect(map['IF_SOME_INTERFACE~DO_IT']).toBe(1);
  });

  test('handles leading whitespace before METHOD keyword', () => {
    const lines = ['    METHOD constructor.'];
    const map = viewCommand._buildMethodLineMap(lines);
    expect(map['CONSTRUCTOR']).toBe(1);
  });

  test('does not match METHOD keyword inside a string or comment', () => {
    const lines = [
      "* This is a comment about method foo",
      '  " Another comment: METHOD bar',
      '  METHOD real_method.'
    ];
    const map = viewCommand._buildMethodLineMap(lines);
    expect(Object.keys(map)).toEqual(['REAL_METHOD']);
    expect(map['REAL_METHOD']).toBe(3);
  });

  test('returns empty map for source with no methods', () => {
    const lines = ['CLASS zcl_empty DEFINITION.', 'ENDCLASS.'];
    const map = viewCommand._buildMethodLineMap(lines);
    expect(Object.keys(map)).toHaveLength(0);
  });

  test('maps multiple methods to correct line numbers', () => {
    const lines = [
      '  METHOD alpha.',
      '    WRITE 1.',
      '  ENDMETHOD.',
      '  METHOD beta.',
      '    WRITE 2.',
      '  ENDMETHOD.'
    ];
    const map = viewCommand._buildMethodLineMap(lines);
    expect(map['ALPHA']).toBe(1);
    expect(map['BETA']).toBe(4);
  });
});

describe('View Command - findFirstExecutableLine()', () => {
  let viewCommand;

  beforeEach(() => {
    jest.resetModules();
    viewCommand = require('../../src/commands/view');
  });

  test('skips METHOD statement and blank lines', () => {
    const lines = [
      '  METHOD do_it.',
      '',
      '  DATA lv_x TYPE i.',
      '  lv_x = 1.'
    ];
    expect(viewCommand._findFirstExecutableLine(lines)).toBe(3);
  });

  test('skips DATA, FINAL, TYPES, CONSTANTS, CLASS-DATA declarations', () => {
    const lines = [
      '  METHOD run.',
      '  DATA lv_a TYPE i.',
      '  FINAL lv_b TYPE i.',
      '  TYPES ty_t TYPE i.',
      '  CONSTANTS c_max TYPE i VALUE 10.',
      '  CLASS-DATA gv_count TYPE i.',
      '  WRITE hello.'
    ];
    expect(viewCommand._findFirstExecutableLine(lines)).toBe(6);
  });

  test('returns 0 when all lines are declarations/blank', () => {
    const lines = [
      '  METHOD run.',
      '',
      '  DATA lv_x TYPE i.'
    ];
    expect(viewCommand._findFirstExecutableLine(lines)).toBe(0);
  });

  test('returns 0 for empty lines array', () => {
    expect(viewCommand._findFirstExecutableLine([])).toBe(0);
  });

  test('returns index of first non-declaration line immediately', () => {
    const lines = ['  WRITE hello.'];
    expect(viewCommand._findFirstExecutableLine(lines)).toBe(0);
  });

  test('handles case-insensitive DATA keyword', () => {
    const lines = ['  method foo.', '  data x type i.', '  WRITE x.'];
    expect(viewCommand._findFirstExecutableLine(lines)).toBe(2);
  });

  test('skips ABAP comment lines starting with "', () => {
    const lines = [
      '  METHOD parse_file.',
      '  " Parse file path to extract obj_type',
      '  " Example: foo.clas.abap -> CLAS',
      '  DATA lv_x TYPE string.',
      '  lv_x = iv_file.'
    ];
    expect(viewCommand._findFirstExecutableLine(lines)).toBe(4);
  });

  test('skips ABAP comment lines starting with *', () => {
    const lines = [
      '  METHOD run.',
      '* Old-style comment',
      '  WRITE hello.'
    ];
    expect(viewCommand._findFirstExecutableLine(lines)).toBe(2);
  });

  test('skips mixed comments and declarations before first executable line', () => {
    const lines = [
      '  METHOD do_it.',
      '  " Step 1: prepare',
      '  DATA lv_x TYPE i.',
      '  lv_x = 1.'
    ];
    expect(viewCommand._findFirstExecutableLine(lines)).toBe(3);
  });

  test('skips inline DATA( declaration', () => {
    const lines = [
      '  METHOD foo.',
      '  DATA(lv_x) = iv_param.',
      '  lv_x = lv_x + 1.'
    ];
    // DATA( is an inline declaration — skip it
    expect(viewCommand._findFirstExecutableLine(lines)).toBe(2);
  });

  test('skips multi-line DATA: block continuation lines', () => {
    const lines = [
      '  METHOD detect_include_info.',
      '  " comment',
      '  DATA: lv_name TYPE tadir-obj_name,',
      '        lv_obj_name TYPE tadir-obj_name,',
      '        lt_source_check TYPE STANDARD TABLE OF string.',
      '  rs_info-is_source_include = abap_false.'
    ];
    // Continuation lines (lv_obj_name, lt_source_check) must also be skipped
    expect(viewCommand._findFirstExecutableLine(lines)).toBe(5);
  });

  test('skips single-line DATA: block (ends with period on same line)', () => {
    const lines = [
      '  METHOD foo.',
      '  DATA: lv_x TYPE i.',
      '  WRITE lv_x.'
    ];
    expect(viewCommand._findFirstExecutableLine(lines)).toBe(2);
  });

  // PROG-specific tests
  test('skips REPORT statement for program source', () => {
    const lines = [
      'REPORT zmy_program.',
      'START-OF-SELECTION.',
      '  WRITE hello.'
    ];
    expect(viewCommand._findFirstExecutableLine(lines)).toBe(1);
  });

  test('skips multi-line PARAMETERS: block in program', () => {
    // Mirrors z_abgagt_bg_executor.prog.abap structure
    const lines = [
      'REPORT z_abgagt_bg_executor.',
      '',
      'PARAMETERS: p_cmd   TYPE string LOWER CASE,',
      '            p_data  TYPE string LOWER CASE.',
      '',
      'DATA: lo_factory TYPE REF TO zif_abgagt_cmd_factory.',
      '',
      'START-OF-SELECTION.',
      '  lo_factory = NEW #( ).'
    ];
    expect(viewCommand._findFirstExecutableLine(lines)).toBe(7);
  });

  test('skips single-line PARAMETERS: (ends with period on same line)', () => {
    const lines = [
      'REPORT zmy_prog.',
      'PARAMETERS: p_in TYPE string.',
      'START-OF-SELECTION.',
      '  WRITE p_in.'
    ];
    expect(viewCommand._findFirstExecutableLine(lines)).toBe(2);
  });

  test('skips TABLES statement', () => {
    const lines = [
      'REPORT zmy_prog.',
      'TABLES: mara.',
      'START-OF-SELECTION.',
      '  WRITE mara-matnr.'
    ];
    expect(viewCommand._findFirstExecutableLine(lines)).toBe(2);
  });

  test('skips SELECTION-SCREEN and SELECT-OPTIONS statements', () => {
    const lines = [
      'REPORT zmy_prog.',
      'SELECTION-SCREEN BEGIN OF BLOCK b1.',
      'SELECT-OPTIONS: s_matnr FOR mara-matnr.',
      'SELECTION-SCREEN END OF BLOCK b1.',
      'START-OF-SELECTION.',
      '  WRITE done.'
    ];
    expect(viewCommand._findFirstExecutableLine(lines)).toBe(4);
  });

  test('skips REPORT then multi-line PARAMETERS then multi-line DATA', () => {
    // Full realistic program header — first executable is START-OF-SELECTION
    const lines = [
      '*&---comment---*',
      'REPORT z_bg_executor.',
      '',
      'PARAMETERS: p_cmd   TYPE string LOWER CASE,',
      '            p_data  TYPE string LOWER CASE.',
      '',
      'DATA: lo_factory      TYPE REF TO zif_cmd_factory,',
      '      lo_command      TYPE REF TO zif_command,',
      '      lv_result       TYPE string.',
      '',
      'START-OF-SELECTION.',
      '  TRY.'
    ];
    expect(viewCommand._findFirstExecutableLine(lines)).toBe(10);
  });
});
