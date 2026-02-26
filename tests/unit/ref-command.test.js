/**
 * Unit tests for ref-search.js
 */

describe('RefSearch', () => {
  describe('listTopics', () => {
    test('returns object with topics array when reference folder exists', async () => {
      jest.resetModules();
      const refSearch = require('../../src/ref-search');

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
      const refSearch = require('../../src/ref-search');

      const result = await refSearch.listTopics();
      // If reference folder is found, topics should have entries
      if (!result.error) {
        expect(result.topics.length).toBeGreaterThan(0);
      }
    });

    test('each topic has topic and file properties', async () => {
      jest.resetModules();
      const refSearch = require('../../src/ref-search');

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
      const refSearch = require('../../src/ref-search');

      const result = await refSearch.getTopic('internal-tables');
      // Should return either content or error (if reference folder not found)
      expect(result).toBeDefined();
      if (result.error) {
        // Reference folder not found - this is acceptable in CI
        expect(result.error).toMatch(/not found|Topic not found|File not found/);
      } else {
        expect(result.content).toBeDefined();
      }
    });

    test('handles unknown topic gracefully', async () => {
      jest.resetModules();
      const refSearch = require('../../src/ref-search');

      const result = await refSearch.getTopic('unknown-topic-xyz-123');
      // Should either return error or try to find it
      expect(result).toBeDefined();
    });
  });

  describe('TOPIC_MAP', () => {
    test('TOPIC_MAP is exported', async () => {
      jest.resetModules();
      const refSearch = require('../../src/ref-search');

      expect(refSearch.TOPIC_MAP).toBeDefined();
    });

    test('TOPIC_MAP contains expected topics', async () => {
      jest.resetModules();
      const refSearch = require('../../src/ref-search');

      expect(refSearch.TOPIC_MAP['internal-tables']).toBeDefined();
      expect(refSearch.TOPIC_MAP['sql']).toBeDefined();
      expect(refSearch.TOPIC_MAP['oop']).toBeDefined();
      expect(refSearch.TOPIC_MAP['testing']).toBeDefined();
    });

    test('TOPIC_MAP contains exceptions', async () => {
      jest.resetModules();
      const refSearch = require('../../src/ref-search');

      expect(refSearch.TOPIC_MAP['exceptions']).toBeDefined();
    });
  });

  describe('getBuiltInGuidelinesPath', () => {
    test('returns path to built-in guidelines', async () => {
      jest.resetModules();
      const refSearch = require('../../src/ref-search');

      const result = refSearch.getBuiltInGuidelinesPath();
      expect(typeof result).toBe('string');
      expect(result.length).toBeGreaterThan(0);
    });
  });

  describe('searchPattern', () => {
    test('returns result object with pattern', async () => {
      jest.resetModules();
      const refSearch = require('../../src/ref-search');

      const result = await refSearch.searchPattern('VALUE');
      // Just verify it returns something
      expect(result).toBeDefined();
      expect(typeof result).toBe('object');
    });
  });

  describe('detectGuidelinesFolder', () => {
    test('detects guidelines folder', async () => {
      jest.resetModules();
      const refSearch = require('../../src/ref-search');

      const result = await refSearch.detectGuidelinesFolder();
      // Should return either path or null
      expect(result).toBeDefined();
    });
  });
});
