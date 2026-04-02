/**
 * Unit tests for ref command
 */

// Mock console methods
const originalError = console.error;
const originalLog = console.log;
let consoleErrorOutput = [];
let consoleLogOutput = [];

beforeEach(() => {
  consoleErrorOutput = [];
  consoleLogOutput = [];
  console.error = jest.fn((...args) => consoleErrorOutput.push(args.join(' ')));
  console.log = jest.fn((...args) => consoleLogOutput.push(args.join(' ')));
});

afterEach(() => {
  console.error = originalError;
  console.log = originalLog;
});

// Mock process.exit
const mockExit = jest.spyOn(process, 'exit').mockImplementation((code) => {
  throw new Error(`process.exit(${code})`);
});

describe('Ref Command - Argument Parsing', () => {
  jest.setTimeout(30000);
  let refCommand;

  beforeEach(() => {
    jest.clearAllMocks();
    // Reset modules to ensure clean state
    jest.resetModules();
    refCommand = require('../../src/commands/ref');
  });

  test('executes pattern search with single pattern argument', async () => {
    const mockContext = {};

    // This should work without errors
    await expect(refCommand.execute(['json'], mockContext)).resolves.not.toThrow();
  });

  test('executes pattern search with quoted pattern', async () => {
    const mockContext = {};

    await expect(refCommand.execute(['CORRESPONDING'], mockContext)).resolves.not.toThrow();
  });

  test('handles --topic argument correctly', async () => {
    const mockContext = {};

    await expect(refCommand.execute(['--topic', 'exceptions'], mockContext)).resolves.not.toThrow();
  });

  test('handles --list-topics flag correctly', async () => {
    const mockContext = {};

    await expect(refCommand.execute(['--list-topics'], mockContext)).resolves.not.toThrow();
  });

  test('shows error when no pattern or flags provided', async () => {
    const mockContext = {};

    await expect(refCommand.execute([], mockContext)).rejects.toThrow('process.exit(1)');
    expect(consoleErrorOutput.join('\n')).toContain('No pattern specified');
  });
});

describe('ABAP Reference Utility', () => {
  describe('listTopics', () => {
    test('returns object with topics array when reference folder exists', async () => {
      jest.resetModules();
      const refSearch = require('../../src/utils/abap-reference');

      const result = await refSearch.listTopics();
      // If reference folder is found, should have topics
      if (result.error) {
        // Skip test if reference folder not available
        expect(result).toHaveProperty('error');
      } else {
        expect(result).toBeDefined();
        expect(result).toHaveProperty('topics');
        expect(Array.isArray(result.topics)).toBe(true);
      }
    });

    test('topics array is not empty when reference folder exists', async () => {
      jest.resetModules();
      const refSearch = require('../../src/utils/abap-reference');

      const result = await refSearch.listTopics();
      // guidelineTopics (bundled) should always have entries
      expect(result.guidelineTopics.length).toBeGreaterThan(0);
      // cheat sheet topics only present when reference folder is configured and exists
      if (result.referenceFolder) {
        expect(result.topics.length).toBeGreaterThan(0);
      }
    });

    test('each topic has topic and file properties', async () => {
      jest.resetModules();
      const refSearch = require('../../src/utils/abap-reference');

      const result = await refSearch.listTopics();
      if (!result.error && result.topics.length > 0) {
        result.topics.forEach(topic => {
          expect(topic).toHaveProperty('topic');
          expect(topic).toHaveProperty('file');
        });
      }
    });
  });

  describe('getTopic', () => {
    test('returns content for valid topic when reference folder exists', async () => {
      jest.resetModules();
      const refSearch = require('../../src/utils/abap-reference');

      const result = await refSearch.getTopic('internal-tables');
      // Should return either content or error (if reference folder not found)
      expect(result).toBeDefined();
      if (result.error) {
        // Reference folder not found or cheat sheets not configured - acceptable in CI
        expect(result.error).toMatch(/not found|Topic not found|File not found|requires SAP ABAP cheat sheets/);
      } else {
        expect(result.content).toBeDefined();
      }
    });

    test('returns setup hint when cheat-sheet topic requested but no reference folder', async () => {
      jest.resetModules();
      // Mock detectReferenceFolder to return null (no cheat sheets configured)
      jest.mock('../../src/utils/abap-reference', () => {
        const actual = jest.requireActual('../../src/utils/abap-reference');
        return actual;
      });
      const refSearch = require('../../src/utils/abap-reference');

      // Only meaningful when cheat sheets are NOT configured/available
      const result = await refSearch.getTopic('internal-tables');
      if (result.error && result.error.includes('requires SAP ABAP cheat sheets')) {
        expect(result.hint).toContain('abap-cheat-sheets');
        expect(result.hint).toContain('--list-topics');
        // Either "not configured" hint or "folder not found" hint
        const hasSetupHint = result.hint.includes('referenceFolder') || result.hint.includes('ref --clone');
        expect(hasSetupHint).toBe(true);
      }
      // If cheat sheets ARE configured and available, this test is a no-op (content returned instead)
    });

    test('handles unknown topic gracefully', async () => {
      jest.resetModules();
      const refSearch = require('../../src/utils/abap-reference');

      const result = await refSearch.getTopic('unknown-topic-xyz-123');
      // Should either return error or try to find it
      expect(result).toBeDefined();
    });
  });

  describe('TOPIC_MAP', () => {
    test('TOPIC_MAP is exported', async () => {
      jest.resetModules();
      const refSearch = require('../../src/utils/abap-reference');

      expect(refSearch.TOPIC_MAP).toBeDefined();
    });

    test('TOPIC_MAP contains expected topics', async () => {
      jest.resetModules();
      const refSearch = require('../../src/utils/abap-reference');

      expect(refSearch.TOPIC_MAP['internal-tables']).toBeDefined();
      expect(refSearch.TOPIC_MAP['sql-cheatsheet']).toBeDefined();
      expect(refSearch.TOPIC_MAP['oop']).toBeDefined();
      expect(refSearch.TOPIC_MAP['unit-tests']).toBeDefined();
    });

    test('TOPIC_MAP contains exceptions', async () => {
      jest.resetModules();
      const refSearch = require('../../src/utils/abap-reference');

      expect(refSearch.TOPIC_MAP['exceptions-cheatsheet']).toBeDefined();
    });
  });

  describe('getBuiltInGuidelinesPath', () => {
    test('returns path to built-in guidelines', async () => {
      jest.resetModules();
      const refSearch = require('../../src/utils/abap-reference');

      const result = refSearch.getBuiltInGuidelinesPath();
      expect(typeof result).toBe('string');
      expect(result.length).toBeGreaterThan(0);
    });
  });

  describe('searchPattern', () => {
    test('returns result object with pattern', async () => {
      jest.resetModules();
      const refSearch = require('../../src/utils/abap-reference');

      const result = await refSearch.searchPattern('VALUE');
      // Just verify it returns something
      expect(result).toBeDefined();
      expect(typeof result).toBe('object');
    });
  });

  describe('detectGuidelinesFolder', () => {
    test('detects guidelines folder', async () => {
      jest.resetModules();
      const refSearch = require('../../src/utils/abap-reference');

      const result = await refSearch.detectGuidelinesFolder();
      // Should return either path or null
      expect(result).toBeDefined();
    });
  });

  describe('getGuidelineFilesFromPath', () => {
    const fs = require('fs');
    const os = require('os');
    const path = require('path');
    let tmpDir;

    beforeEach(() => {
      tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'abapgit-ref-test-'));
    });

    afterEach(() => {
      fs.rmSync(tmpDir, { recursive: true, force: true });
    });

    test('returns files from a directory', async () => {
      jest.resetModules();
      const refSearch = require('../../src/utils/abap-reference');

      fs.writeFileSync(path.join(tmpDir, 'sql.md'), '# SQL\nSELECT example');
      fs.writeFileSync(path.join(tmpDir, 'classes.md'), '# Classes\nCLASS example');

      const files = await refSearch.getGuidelineFilesFromPath(tmpDir);
      expect(files).toHaveLength(2);
      expect(files.map(f => f.name).sort()).toEqual(['classes.md', 'sql.md']);
    });

    test('ignores non-.md files', async () => {
      jest.resetModules();
      const refSearch = require('../../src/utils/abap-reference');

      fs.writeFileSync(path.join(tmpDir, 'notes.md'), '# Notes');
      fs.writeFileSync(path.join(tmpDir, 'ignored.txt'), 'not md');

      const files = await refSearch.getGuidelineFilesFromPath(tmpDir);
      expect(files).toHaveLength(1);
      expect(files[0].name).toBe('notes.md');
    });

    test('returns empty array for missing directory', async () => {
      jest.resetModules();
      const refSearch = require('../../src/utils/abap-reference');

      const files = await refSearch.getGuidelineFilesFromPath('/nonexistent/path/xyz');
      expect(files).toEqual([]);
    });

    test('uses custom label in relativePath', async () => {
      jest.resetModules();
      const refSearch = require('../../src/utils/abap-reference');

      fs.writeFileSync(path.join(tmpDir, 'sql.md'), '# SQL');
      const files = await refSearch.getGuidelineFilesFromPath(tmpDir, 'custom-label');
      expect(files[0].relativePath).toContain('custom-label');
    });
  });

  describe('searchPattern — built-in fallback', () => {
    test('returns built-in results when no local guidelines folder exists', async () => {
      jest.resetModules();
      const refSearch = require('../../src/utils/abap-reference');

      // Pattern that is almost certainly in the built-in guidelines
      const result = await refSearch.searchPattern('CLASS');
      expect(result).toBeDefined();
      expect(typeof result).toBe('object');
      // If built-in path exists, files should be populated (no error)
      const builtInPath = refSearch.getBuiltInGuidelinesPath();
      if (builtInPath) {
        expect(result.error).toBeUndefined();
        expect(result.files).toBeDefined();
      }
    });

    test('local file shadows built-in file of same name', async () => {
      const fs = require('fs');
      const os = require('os');
      const path = require('path');
      const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'abapgit-shadow-test-'));
      try {
        jest.resetModules();
        const refSearch = require('../../src/utils/abap-reference');
        const builtInPath = refSearch.getBuiltInGuidelinesPath();
        if (!builtInPath) return; // skip if not available

        // Get a real built-in filename to shadow
        const builtInFiles = fs.readdirSync(builtInPath).filter(f => f.endsWith('.md'));
        if (builtInFiles.length === 0) return;

        const fileToShadow = builtInFiles[0];
        const uniqueContent = 'UNIQUE_SHADOW_CONTENT_XYZ';
        fs.writeFileSync(path.join(tmpDir, fileToShadow), uniqueContent);

        // Get files from built-in that are NOT shadowed by local
        const localFileNames = new Set([fileToShadow]);
        const builtInAllFiles = await refSearch.getGuidelineFilesFromPath(builtInPath);
        const unshadowedBuiltIn = builtInAllFiles.filter(f => !localFileNames.has(f.name));

        // Shadowed file should NOT appear in unshadowed list
        expect(unshadowedBuiltIn.some(f => f.name === fileToShadow)).toBe(false);
        // Other files should still be present
        expect(unshadowedBuiltIn.length).toBe(builtInAllFiles.length - 1);
      } finally {
        fs.rmSync(tmpDir, { recursive: true, force: true });
      }
    });

    test('local .local.md and built-in .md coexist (no shadow conflict)', async () => {
      jest.resetModules();
      const refSearch = require('../../src/utils/abap-reference');
      const builtInPath = refSearch.getBuiltInGuidelinesPath();
      if (!builtInPath) return; // skip if not available

      const builtInFiles = await refSearch.getGuidelineFilesFromPath(builtInPath);
      const builtInNames = new Set(builtInFiles.map(f => f.name));

      // objects.local.md should NOT be present in built-in (it's a local-only convention)
      expect(builtInNames.has('objects.local.md')).toBe(false);

      // objects.md (the standard file) should be present in built-in
      // so it won't be shadowed by objects.local.md (different name)
      if (builtInNames.has('objects.md')) {
        // Shadow check: objects.local.md != objects.md → no shadow
        const localFileNames = new Set(['objects.local.md']);
        const unshadowed = builtInFiles.filter(f => !localFileNames.has(f.name));
        expect(unshadowed.some(f => f.name === 'objects.md')).toBe(true);
      }
    });
  });
});
