'use strict';

jest.mock('fs', () => ({
  existsSync: jest.fn(() => true),
  readFileSync: jest.fn((p) => p.includes('.abapgit-agent.json') ? '{}' : 'mock content'),
  readdirSync: jest.fn(() => []),
  statSync: jest.fn(() => ({ isFile: () => true }))
}));

jest.mock('path', () => ({
  isAbsolute: jest.fn(() => false),
  join: jest.fn((...args) => args.join('/')),
  resolve: jest.fn((...args) => '/' + args.join('/')),
  basename: jest.fn((p) => p.split('/').pop()),
  dirname: jest.fn((p) => p.split('/').slice(0, -1).join('/'))
}));

describe('hana command', () => {
  let hanaCommand;
  let mockHttp;
  let mockContext;
  let consoleOutput;
  let consoleErrors;

  beforeEach(() => {
    jest.resetModules();
    jest.spyOn(process, 'exit').mockImplementation((code) => { throw new Error(`process.exit(${code})`); });
    consoleOutput = [];
    consoleErrors = [];
    jest.spyOn(console, 'log').mockImplementation((...args) => consoleOutput.push(args.join(' ')));
    jest.spyOn(console, 'error').mockImplementation((...args) => consoleErrors.push(args.join(' ')));

    mockHttp = {
      post: jest.fn(),
      fetchCsrfToken: jest.fn().mockResolvedValue('mock-csrf')
    };

    mockContext = {
      loadConfig: jest.fn().mockReturnValue({ host: 'test.sap.com', user: 'DEV' }),
      AbapHttp: jest.fn().mockImplementation(() => mockHttp)
    };

    hanaCommand = require('../../src/commands/hana');
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  test('command has correct metadata', () => {
    expect(hanaCommand.name).toBe('hana');
    expect(hanaCommand.requiresAbapConfig).toBe(true);
  });

  test('top-level --help prints usage', async () => {
    await hanaCommand.execute(['--help'], mockContext);
    expect(consoleOutput.some(l => l.includes('view'))).toBe(true);
    expect(consoleOutput.some(l => l.includes('schema'))).toBe(true);
  });

  test('unknown subcommand prints error and exits', async () => {
    await expect(hanaCommand.execute(['bogus'], mockContext)).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('bogus'))).toBe(true);
  });

  // ── hana list ──────────────────────────────────────────────────────────────

  test('hana list --help prints usage', async () => {
    await hanaCommand.execute(['view', 'list', '--help'], mockContext);
    expect(consoleOutput.some(l => l.includes('--package'))).toBe(true);
  });

  test('hana list requires --package', async () => {
    await expect(hanaCommand.execute(['view', 'list'], mockContext)).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('--package'))).toBe(true);
  });

  test('hana list prints table for successful response', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      package: 'com.example.myapp',
      objects: [
        { name: 'SalesView',    suffix: 'attributeview',   deploy_state: 'deployed',     hot_status: 'A' },
        { name: 'ComputeScore', suffix: 'calculationview', deploy_state: 'not_deployed', hot_status: 'I' }
      ]
    });

    await hanaCommand.execute(['view', 'list', '--package', 'com.example.myapp'], mockContext);

    expect(mockHttp.post).toHaveBeenCalledWith(
      expect.stringContaining('/hana'),
      expect.objectContaining({ action: 'list', hanaPackage: 'com.example.myapp' })
    );
    const output = consoleOutput.join('\n');
    expect(output).toContain('SalesView');
    expect(output).toContain('deployed');
    expect(output).toContain('not_deployed');
    expect(output).toContain('1 not deployed');
  });

  test('hana list shows empty package message when no objects', async () => {
    mockHttp.post.mockResolvedValue({ success: true, package: 'com.example.empty', objects: [] });

    await hanaCommand.execute(['view', 'list', '--package', 'com.example.empty'], mockContext);

    expect(consoleOutput.some(l => l.includes('No objects found'))).toBe(true);
  });

  test('hana list --json outputs raw JSON', async () => {
    const resp = { success: true, package: 'com.example.myapp', objects: [] };
    mockHttp.post.mockResolvedValue(resp);

    await hanaCommand.execute(['view', 'list', '--package', 'com.example.myapp', '--json'], mockContext);

    const jsonLine = consoleOutput.find(l => { try { JSON.parse(l); return true; } catch { return false; } });
    expect(jsonLine).toBeDefined();
    expect(JSON.parse(jsonLine)).toMatchObject({ success: true });
  });

  test('hana list prints error when response has error field', async () => {
    mockHttp.post.mockResolvedValue({ success: false, error: 'package_not_found' });

    await expect(hanaCommand.execute(['view', 'list', '--package', 'com.bad'], mockContext))
      .rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('package_not_found'))).toBe(true);
  });

  test('hana list passes subpackages flag', async () => {
    mockHttp.post.mockResolvedValue({ success: true, package: 'com.example', objects: [] });

    await hanaCommand.execute(['view', 'list', '--package', 'com.example', '--subpackages'], mockContext);

    expect(mockHttp.post).toHaveBeenCalledWith(
      expect.any(String),
      expect.objectContaining({ subpackages: true })
    );
  });

  // ── hana schema ────────────────────────────────────────────────────────────

  test('hana schema --help prints usage', async () => {
    await hanaCommand.execute(['schema', '--help'], mockContext);
    expect(consoleOutput.some(l => l.includes('schema list'))).toBe(true);
    expect(consoleOutput.some(l => l.includes('schema tables'))).toBe(true);
    expect(consoleOutput.some(l => l.includes('schema fields'))).toBe(true);
  });

  test('hana schema with unknown subcommand exits', async () => {
    await expect(hanaCommand.execute(['schema', 'bogus'], mockContext)).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('bogus'))).toBe(true);
  });

  test('hana schema list posts action=schema, subaction=list', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      subaction: 'list',
      rows: [{ schema_name: 'SAP_FRA_CORE_S4' }, { schema_name: 'SYS' }]
    });

    await hanaCommand.execute(['schema', 'list'], mockContext);

    expect(mockHttp.post).toHaveBeenCalledWith(
      expect.stringContaining('/hana'),
      expect.objectContaining({ action: 'schema', subaction: 'list' })
    );
    const output = consoleOutput.join('\n');
    expect(output).toContain('SAP_FRA_CORE_S4');
    expect(output).toContain('SYS');
    expect(output).toContain('2 schema(s)');
  });

  test('hana schema list passes --filter and --limit', async () => {
    mockHttp.post.mockResolvedValue({ success: true, rows: [] });

    await hanaCommand.execute(['schema', 'list', '--filter', '%CORE%', '--limit', '50'], mockContext);

    expect(mockHttp.post).toHaveBeenCalledWith(
      expect.any(String),
      expect.objectContaining({ filter: '%CORE%', limit: 50 })
    );
  });

  test('hana schema tables requires --schema', async () => {
    await expect(hanaCommand.execute(['schema', 'tables'], mockContext)).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('--schema'))).toBe(true);
  });

  test('hana schema tables prints table for response', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      subaction: 'tables',
      rows: [
        { table_name: 'T001',  is_column_table: 'TRUE' },
        { table_name: 'T001W', is_column_table: 'TRUE' }
      ]
    });

    await hanaCommand.execute(['schema', 'tables', '--schema', 'sap_fra_core_s4'], mockContext);

    expect(mockHttp.post).toHaveBeenCalledWith(
      expect.any(String),
      expect.objectContaining({ action: 'schema', subaction: 'tables', schema: 'sap_fra_core_s4' })
    );
    const output = consoleOutput.join('\n');
    expect(output).toContain('T001');
    expect(output).toContain('T001W');
    expect(output).toContain('2 table(s)');
  });

  test('hana schema fields requires --schema and --table', async () => {
    await expect(hanaCommand.execute(['schema', 'fields', '--schema', 'X'], mockContext)).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('--table'))).toBe(true);
  });

  test('hana schema fields prints columns', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      subaction: 'fields',
      rows: [
        { position: 1, column_name: 'MANDT', data_type_name: 'NVARCHAR', length: 3,  scale: 0, is_nullable: 'FALSE' },
        { position: 2, column_name: 'BUKRS', data_type_name: 'NVARCHAR', length: 4,  scale: 0, is_nullable: 'FALSE' },
        { position: 3, column_name: 'BUTXT', data_type_name: 'NVARCHAR', length: 25, scale: 0, is_nullable: 'TRUE'  }
      ]
    });

    await hanaCommand.execute(['schema', 'fields', '--schema', 'SAP_FRA_CORE_S4', '--table', 'T001'], mockContext);

    const output = consoleOutput.join('\n');
    expect(output).toContain('MANDT');
    expect(output).toContain('BUKRS');
    expect(output).toContain('BUTXT');
    expect(output).toContain('3 column(s)');
  });

  test('hana schema --json bypasses table formatting', async () => {
    const resp = { success: true, subaction: 'list', rows: [{ schema_name: 'SYS' }] };
    mockHttp.post.mockResolvedValue(resp);

    await hanaCommand.execute(['schema', 'list', '--json'], mockContext);

    const jsonLine = consoleOutput.find(l => { try { JSON.parse(l); return true; } catch { return false; } });
    expect(jsonLine).toBeDefined();
    expect(JSON.parse(jsonLine)).toMatchObject({ success: true, subaction: 'list' });
  });

  test('hana schema empty rows prints "No matches found"', async () => {
    mockHttp.post.mockResolvedValue({ success: true, rows: [] });

    await hanaCommand.execute(['schema', 'tables', '--schema', 'X'], mockContext);

    expect(consoleOutput.some(l => l.includes('No matches found'))).toBe(true);
  });

  test('hana schema surfaces backend error', async () => {
    mockHttp.post.mockResolvedValue({ success: false, error: 'schema is required' });

    await expect(hanaCommand.execute(['schema', 'tables', '--schema', 'X'], mockContext))
      .rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('schema is required'))).toBe(true);
  });

  // ── hana view drop ─────────────────────────────────────────────────────────

  test('hana view drop --help prints usage', async () => {
    await hanaCommand.execute(['view', 'drop', '--help'], mockContext);
    expect(consoleOutput.some(l => l.includes('--file'))).toBe(true);
    expect(consoleOutput.some(l => l.includes('local file'))).toBe(true);
  });

  test('hana view drop requires --file', async () => {
    await expect(hanaCommand.execute(['view', 'drop'], mockContext)).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('--file is required'))).toBe(true);
  });

  test('hana view drop fails when --file path does not exist', async () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValueOnce(false);
    await expect(
      hanaCommand.execute(['view', 'drop', '--file', 'hana/tmp/abgagt/Missing.calculationview'], mockContext)
    ).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('File not found'))).toBe(true);
  });

  test('hana view drop rejects unsupported file types', async () => {
    await expect(
      hanaCommand.execute(['view', 'drop', '--file', 'hana/tmp/abgagt/Foo.hdbview'], mockContext)
    ).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('Unsupported file type'))).toBe(true);
  });

  test('hana view drop posts action=drop with parsed path', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      deployed: 1,
      results: [{ filename: 'ABGAGT_IT_TEST.calculationview', type: 'calculationview', status: 'dropped' }]
    });

    await hanaCommand.execute(
      ['view', 'drop', '--file', 'hana/tmp/abgagt/ABGAGT_IT_TEST.calculationview'],
      mockContext
    );

    expect(mockHttp.post).toHaveBeenCalledWith(
      expect.stringContaining('/hana'),
      expect.objectContaining({
        action:       'drop',
        hanaPackage:  'tmp.abgagt',
        objectName:   'ABGAGT_IT_TEST',
        objectSuffix: 'calculationview',
        dryRun:       false
      })
    );
    const output = consoleOutput.join('\n');
    expect(output).toContain('Dropped 1 object');
    expect(output).toContain('Local file kept');
  });

  test('hana view drop --dry-run sends dryRun: true', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      dry_run: true,
      deployed: 1,
      results: [{ filename: 'ABGAGT_IT_TEST.calculationview', type: 'calculationview', status: 'would_drop' }]
    });

    await hanaCommand.execute(
      ['view', 'drop', '--file', 'hana/tmp/abgagt/ABGAGT_IT_TEST.calculationview', '--dry-run'],
      mockContext
    );

    expect(mockHttp.post).toHaveBeenCalledWith(expect.any(String), expect.objectContaining({ dryRun: true }));
    const output = consoleOutput.join('\n');
    expect(output).toContain('would be dropped');
  });

  test('hana view drop surfaces "already absent" as success', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      results: [{ filename: 'Gone.calculationview', type: 'calculationview', status: 'skipped', message: 'not found in HOT' }]
    });

    await hanaCommand.execute(
      ['view', 'drop', '--file', 'hana/tmp/abgagt/Gone.calculationview'],
      mockContext
    );

    expect(consoleOutput.some(l => l.includes('already absent'))).toBe(true);
  });

  test('hana view drop exits 1 on backend error', async () => {
    mockHttp.post.mockResolvedValue({
      success: false,
      error: 'drop_failed',
      results: [{ filename: 'Broken.calculationview', type: 'calculationview', status: 'failed', message: 'foo' }]
    });

    await expect(
      hanaCommand.execute(['view', 'drop', '--file', 'hana/tmp/abgagt/Broken.calculationview'], mockContext)
    ).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('drop_failed'))).toBe(true);
  });

  // ── hana view preview ──────────────────────────────────────────────────────

  test('hana view preview --help prints usage', async () => {
    await hanaCommand.execute(['view', 'preview', '--help'], mockContext);
    expect(consoleOutput.some(l => l.includes('--file'))).toBe(true);
    expect(consoleOutput.some(l => l.includes('--limit'))).toBe(true);
    expect(consoleOutput.some(l => l.includes('--where'))).toBe(true);
  });

  test('hana view preview requires --file', async () => {
    await expect(hanaCommand.execute(['view', 'preview'], mockContext)).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('--file is required'))).toBe(true);
  });

  test('hana view preview fails when --file path does not exist', async () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValueOnce(false);
    await expect(
      hanaCommand.execute(['view', 'preview', '--file', 'hana/tmp/abgagt/Missing.calculationview'], mockContext)
    ).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('File not found'))).toBe(true);
  });

  test('hana view preview rejects unsupported file types', async () => {
    await expect(
      hanaCommand.execute(['view', 'preview', '--file', 'hana/tmp/abgagt/Foo.hdbview'], mockContext)
    ).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('Unsupported file type'))).toBe(true);
  });

  test('hana view preview posts action=preview with parsed path + defaults', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      view: 'tmp.abgagt/ABGAGT_IT_TEST',
      columns: [
        { name: 'MESSAGE', type: 'NVARCHAR', length: 100 },
        { name: 'DUMMY_MEASURE', type: 'INTEGER', length: null }
      ],
      rows: [{ cells: ['integration test', '1'] }],
      row_count: 1,
      limit: 100,
      offset: 0
    });

    await hanaCommand.execute(
      ['view', 'preview', '--file', 'hana/tmp/abgagt/ABGAGT_IT_TEST.calculationview'],
      mockContext
    );

    expect(mockHttp.post).toHaveBeenCalledWith(
      expect.stringContaining('/hana'),
      expect.objectContaining({
        action:       'preview',
        hanaPackage:  'tmp.abgagt',
        objectName:   'ABGAGT_IT_TEST',
        objectSuffix: 'calculationview',
        limit:        0,
        offset:       0,
        where:        '',
        columns:      []
      })
    );
    const output = consoleOutput.join('\n');
    expect(output).toContain('tmp.abgagt/ABGAGT_IT_TEST');
    expect(output).toContain('MESSAGE');
    expect(output).toContain('integration test');
    expect(output).toContain('1 row(s) returned');
  });

  test('hana view preview propagates --limit, --offset, --where, --columns', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      view: 'tmp.abgagt/V',
      columns: [{ name: 'A', type: 'NVARCHAR', length: 10 }],
      rows: [],
      row_count: 0,
      limit: 50,
      offset: 100
    });

    await hanaCommand.execute(
      ['view', 'preview', '--file', 'hana/tmp/abgagt/V.calculationview',
       '--limit', '50', '--offset', '100',
       '--where', "MESSAGE = 'x'",
       '--columns', 'A,B,C'],
      mockContext
    );

    expect(mockHttp.post).toHaveBeenCalledWith(
      expect.any(String),
      expect.objectContaining({
        limit:   50,
        offset:  100,
        where:   "MESSAGE = 'x'",
        columns: ['A', 'B', 'C']
      })
    );
  });

  test('hana view preview --json emits raw JSON', async () => {
    const resp = {
      success: true,
      view: 'tmp.abgagt/V',
      columns: [{ name: 'A', type: 'NVARCHAR', length: 10 }],
      rows: [{ cells: ['val'] }],
      row_count: 1
    };
    mockHttp.post.mockResolvedValue(resp);

    await hanaCommand.execute(
      ['view', 'preview', '--file', 'hana/tmp/abgagt/V.calculationview', '--json'],
      mockContext
    );

    const jsonLine = consoleOutput.find(l => { try { JSON.parse(l); return true; } catch { return false; } });
    expect(jsonLine).toBeDefined();
    expect(JSON.parse(jsonLine)).toMatchObject({ success: true, view: 'tmp.abgagt/V' });
  });

  test('hana view preview empty result prints "No rows returned"', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      view: 'tmp.abgagt/V',
      columns: [{ name: 'A', type: 'NVARCHAR', length: 10 }],
      rows: [],
      row_count: 0,
      limit: 100,
      offset: 0
    });

    await hanaCommand.execute(
      ['view', 'preview', '--file', 'hana/tmp/abgagt/V.calculationview'],
      mockContext
    );
    expect(consoleOutput.some(l => l.includes('No rows returned'))).toBe(true);
  });

  test('hana view preview surfaces view_not_active error', async () => {
    mockHttp.post.mockResolvedValue({
      success: false,
      error: 'view_not_active',
      message: 'View "tmp.abgagt/V" is not active in HANA.'
    });

    await expect(
      hanaCommand.execute(['view', 'preview', '--file', 'hana/tmp/abgagt/V.calculationview'], mockContext)
    ).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('view_not_active'))).toBe(true);
    expect(consoleErrors.some(l => l.includes('not active in HANA'))).toBe(true);
  });

  test('hana view preview --vertical renders one column per line', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      view: 'tmp.abgagt/V',
      columns: [
        { name: 'MESSAGE',       type: 'NVARCHAR', length: 100 },
        { name: 'DUMMY_MEASURE', type: 'INTEGER',  length: null }
      ],
      rows: [{ cells: ['hello', '7'] }],
      row_count: 1,
      limit: 100,
      offset: 0
    });

    await hanaCommand.execute(
      ['view', 'preview', '--file', 'hana/tmp/abgagt/V.calculationview', '--vertical'],
      mockContext
    );

    const output = consoleOutput.join('\n');
    expect(output).toContain('Row 1');
    expect(output).toMatch(/MESSAGE\s+:\s+hello/);
    expect(output).toMatch(/DUMMY_MEASURE\s+:\s+7/);
  });

  // ── hana table preview ─────────────────────────────────────────────────────

  test('hana table preview --help prints usage', async () => {
    await hanaCommand.execute(['table', 'preview', '--help'], mockContext);
    expect(consoleOutput.some(l => l.includes('--schema'))).toBe(true);
    expect(consoleOutput.some(l => l.includes('--logical-schema'))).toBe(true);
    expect(consoleOutput.some(l => l.includes('--table'))).toBe(true);
  });

  test('hana table preview requires --table', async () => {
    await expect(
      hanaCommand.execute(['table', 'preview', '--schema', 'SYS'], mockContext)
    ).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('--table is required'))).toBe(true);
  });

  test('hana table preview requires exactly one of --schema or --logical-schema', async () => {
    await expect(
      hanaCommand.execute(['table', 'preview', '--table', 'T001'], mockContext)
    ).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('--schema') && l.includes('--logical-schema'))).toBe(true);
  });

  test('hana table preview rejects both --schema and --logical-schema', async () => {
    await expect(
      hanaCommand.execute(
        ['table', 'preview', '--schema', 'X', '--logical-schema', 'L', '--table', 'T'],
        mockContext
      )
    ).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('mutually exclusive'))).toBe(true);
  });

  test('hana table preview posts action=tablePreview with physical --schema', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      table: 'SYS.DUMMY',
      logical: '',
      columns: [{ name: 'DUMMY', type: 'NVARCHAR', length: 1 }],
      rows: [{ cells: ['X'] }],
      row_count: 1,
      limit: 100,
      offset: 0
    });

    await hanaCommand.execute(
      ['table', 'preview', '--schema', 'SYS', '--table', 'DUMMY'],
      mockContext
    );

    expect(mockHttp.post).toHaveBeenCalledWith(
      expect.stringContaining('/hana'),
      expect.objectContaining({
        action:        'tablePreview',
        schema:        'SYS',
        logicalSchema: '',
        table:         'DUMMY',
        limit:         0,
        offset:        0,
        where:         '',
        columns:       []
      })
    );
    expect(consoleOutput.some(l => l.includes('SYS.DUMMY'))).toBe(true);
    expect(consoleOutput.some(l => l.includes('1 row(s) returned'))).toBe(true);
  });

  test('hana table preview --logical-schema sends logicalSchema, empty schema', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      table: 'FRA_CORE_ERP.T001',
      logical: 'FRA_CORE_ERP',
      columns: [{ name: 'BUKRS', type: 'NVARCHAR', length: 4 }],
      rows: [{ cells: ['1000'] }],
      row_count: 1
    });

    await hanaCommand.execute(
      ['table', 'preview', '--logical-schema', 'FRA_CORE_ERP', '--table', 'T001'],
      mockContext
    );

    expect(mockHttp.post).toHaveBeenCalledWith(
      expect.any(String),
      expect.objectContaining({
        schema:        '',
        logicalSchema: 'FRA_CORE_ERP',
        table:         'T001'
      })
    );
  });

  test('hana table preview propagates --limit, --offset, --where, --columns', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      table: 'SAP_FRA_CORE_S4.BKPF',
      columns: [{ name: 'BUKRS', type: 'NVARCHAR', length: 4 }],
      rows: [],
      row_count: 0,
      limit: 50,
      offset: 100
    });

    await hanaCommand.execute(
      ['table', 'preview',
       '--schema', 'SAP_FRA_CORE_S4', '--table', 'BKPF',
       '--limit', '50', '--offset', '100',
       '--where', "BUKRS = '1000'",
       '--columns', 'BUKRS,GJAHR'],
      mockContext
    );

    expect(mockHttp.post).toHaveBeenCalledWith(
      expect.any(String),
      expect.objectContaining({
        limit:   50,
        offset:  100,
        where:   "BUKRS = '1000'",
        columns: ['BUKRS', 'GJAHR']
      })
    );
  });

  test('hana table preview surfaces table_not_found with a hint', async () => {
    mockHttp.post.mockResolvedValue({
      success: false,
      error: 'table_not_found',
      message: 'Table FOO not found in schema SAP_FRA_CORE_S4.'
    });

    await expect(
      hanaCommand.execute(
        ['table', 'preview', '--schema', 'SAP_FRA_CORE_S4', '--table', 'FOO'],
        mockContext
      )
    ).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('table_not_found'))).toBe(true);
    expect(consoleErrors.some(l => l.includes('hana schema tables'))).toBe(true);
  });

  test('hana table preview surfaces schema_not_mapped hint for logical schema', async () => {
    mockHttp.post.mockResolvedValue({
      success: false,
      error: 'schema_not_mapped',
      message: 'Logical schema "FRA_CORE_ERP" is not mapped.'
    });

    await expect(
      hanaCommand.execute(
        ['table', 'preview', '--logical-schema', 'FRA_CORE_ERP', '--table', 'T001'],
        mockContext
      )
    ).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('schema_not_mapped'))).toBe(true);
    expect(consoleErrors.some(l => l.includes('DB_SCHEMA_MAP'))).toBe(true);
  });

  test('hana table preview --json emits raw JSON', async () => {
    const resp = {
      success: true,
      table: 'SYS.DUMMY',
      columns: [{ name: 'DUMMY', type: 'NVARCHAR', length: 1 }],
      rows: [{ cells: ['X'] }],
      row_count: 1
    };
    mockHttp.post.mockResolvedValue(resp);

    await hanaCommand.execute(
      ['table', 'preview', '--schema', 'SYS', '--table', 'DUMMY', '--json'],
      mockContext
    );

    const jsonLine = consoleOutput.find(l => { try { JSON.parse(l); return true; } catch { return false; } });
    expect(jsonLine).toBeDefined();
    expect(JSON.parse(jsonLine)).toMatchObject({ success: true, table: 'SYS.DUMMY' });
  });

  test('hana table preview empty result prints "No rows returned"', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      table: 'SAP_FRA_CORE_S4.T001',
      columns: [{ name: 'BUKRS', type: 'NVARCHAR', length: 4 }],
      rows: [],
      row_count: 0,
      limit: 100,
      offset: 0
    });

    await hanaCommand.execute(
      ['table', 'preview', '--schema', 'SAP_FRA_CORE_S4', '--table', 'T001'],
      mockContext
    );
    expect(consoleOutput.some(l => l.includes('No rows returned'))).toBe(true);
  });

  test('hana table preview --vertical renders one column per line', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      table: 'SAP_FRA_CORE_S4.T001',
      columns: [
        { name: 'BUKRS', type: 'NVARCHAR', length: 4 },
        { name: 'BUTXT', type: 'NVARCHAR', length: 25 }
      ],
      rows: [{ cells: ['1000', 'ACME'] }],
      row_count: 1,
      limit: 100,
      offset: 0
    });

    await hanaCommand.execute(
      ['table', 'preview',
       '--schema', 'SAP_FRA_CORE_S4', '--table', 'T001', '--vertical'],
      mockContext
    );

    const output = consoleOutput.join('\n');
    expect(output).toContain('Row 1');
    expect(output).toMatch(/BUKRS\s+:\s+1000/);
    expect(output).toMatch(/BUTXT\s+:\s+ACME/);
  });
});
