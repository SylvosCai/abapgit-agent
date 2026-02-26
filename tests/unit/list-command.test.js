/**
 * Unit tests for list command - ABAPGitAgent
 */

// Mock AbapHttp
const mockFetchCsrfToken = jest.fn();
const mockPost = jest.fn();

jest.mock('../../src/utils/abap-http', () => ({
  AbapHttp: jest.fn().mockImplementation(() => ({
    fetchCsrfToken: mockFetchCsrfToken,
    post: mockPost
  }))
}));

// Mock config
jest.mock('../../src/config', () => ({
  getAbapConfig: jest.fn(() => ({
    host: 'test.sap.com',
    sapport: 44300,
    user: 'testuser',
    password: 'testpass',
    client: '100',
    language: 'EN'
  }))
}));

// Mock logger
jest.mock('../../src/logger', () => ({
  info: jest.fn(),
  error: jest.fn(),
  warn: jest.fn(),
  debug: jest.fn()
}));

beforeEach(() => {
  jest.clearAllMocks();
  mockFetchCsrfToken.mockResolvedValue('test-csrf-token');
});

describe('List Command - ABAPGitAgent', () => {
  let agent;
  let ABAPGitAgent;

  beforeEach(() => {
    jest.resetModules();
    ABAPGitAgent = require('../../src/agent').ABAPGitAgent;
    agent = new ABAPGitAgent();
  });

  describe('agent.list', () => {
    test('returns success=true with objects list', async () => {
      mockPost.mockResolvedValue({
        SUCCESS: 'X',
        PACKAGE: '$ZTEST',
        OBJECTS: [
          { TYPE: 'CLAS', NAME: 'ZCL_CLASS1' },
          { TYPE: 'CLAS', NAME: 'ZCL_CLASS2' },
          { TYPE: 'INTF', NAME: 'ZIF_INTERFACE1' }
        ],
        BY_TYPE: [
          { TYPE: 'CLAS', COUNT: 2 },
          { TYPE: 'INTF', COUNT: 1 }
        ],
        TOTAL: 3
      });

      const result = await agent.list('$ZTEST');

      expect(result.success).toBe(true);
      expect(result.package).toBe('$ZTEST');
      expect(result.objects).toHaveLength(3);
      expect(result.by_type).toHaveLength(2);
      expect(result.total).toBe(3);
      expect(result.objects[0].type).toBe('CLAS');
      expect(result.objects[0].name).toBe('ZCL_CLASS1');
    });

    test('returns empty arrays when no objects found', async () => {
      mockPost.mockResolvedValue({
        SUCCESS: 'X',
        PACKAGE: '$ZTEST',
        OBJECTS: [],
        BY_TYPE: [],
        TOTAL: 0
      });

      const result = await agent.list('$ZTEST');

      expect(result.success).toBe(true);
      expect(result.objects).toHaveLength(0);
      expect(result.total).toBe(0);
    });

    test('filters by type when specified', async () => {
      mockPost.mockResolvedValue({
        SUCCESS: 'X',
        OBJECTS: [
          { TYPE: 'CLAS', NAME: 'ZCL_CLASS1' },
          { TYPE: 'CLAS', NAME: 'ZCL_CLASS2' }
        ],
        TOTAL: 2
      });

      await agent.list('$ZTEST', 'CLAS');

      expect(mockPost).toHaveBeenCalledWith(
        '/sap/bc/z_abapgit_agent/list',
        expect.objectContaining({
          package: '$ZTEST',
          type: 'CLAS'
        }),
        expect.any(Object)
      );
    });

    test('filters by name pattern when specified', async () => {
      mockPost.mockResolvedValue({
        SUCCESS: 'X',
        OBJECTS: [],
        TOTAL: 0
      });

      await agent.list('$ZTEST', null, 'ZCL_*');

      expect(mockPost).toHaveBeenCalledWith(
        '/sap/bc/z_abapgit_agent/list',
        expect.objectContaining({
          package: '$ZTEST',
          name: 'ZCL_*'
        }),
        expect.any(Object)
      );
    });

    test('handles custom limit and offset', async () => {
      mockPost.mockResolvedValue({
        SUCCESS: 'X',
        OBJECTS: [],
        TOTAL: 0
      });

      await agent.list('$ZTEST', null, null, 50, 10);

      expect(mockPost).toHaveBeenCalledWith(
        '/sap/bc/z_abapgit_agent/list',
        expect.objectContaining({
          limit: 50,
          offset: 10
        }),
        expect.any(Object)
      );
    });

    test('normalizes uppercase keys from ABAP', async () => {
      mockPost.mockResolvedValue({
        SUCCESS: 'X',
        OBJECTS: [
          { TYPE: 'CLAS', NAME: 'ZCL_TEST' }
        ],
        BY_TYPE: [
          { TYPE: 'CLAS', COUNT: 1 }
        ],
        TOTAL: 1
      });

      const result = await agent.list('$ZTEST');

      expect(result.objects[0].type).toBe('CLAS');
      expect(result.objects[0].name).toBe('ZCL_TEST');
      expect(result.by_type[0].type).toBe('CLAS');
      expect(result.by_type[0].count).toBe(1);
    });

    test('throws error when list fails', async () => {
      mockPost.mockRejectedValue(new Error('Connection failed'));

      await expect(agent.list('$ZTEST')).rejects.toThrow('List command failed');
    });
  });
});
