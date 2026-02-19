/**
 * Unit tests for list command - ABAPGitAgent
 */

// Create mock functions
const mockList = jest.fn();

// Mock abap-client before requiring agent
jest.mock('../src/abap-client', () => ({
  getClient: jest.fn(() => ({
    list: mockList
  }))
}));

// Clear module cache and require fresh
beforeEach(() => {
  jest.clearAllMocks();
});

describe('List Command - ABAPGitAgent', () => {
  let agent;
  let ABAPGitAgent;

  beforeEach(() => {
    jest.resetModules();
    // Re-setup mocks after reset
    jest.doMock('../src/abap-client', () => ({
      getClient: jest.fn(() => ({
        list: mockList
      }))
    }));

    ABAPGitAgent = require('../src/agent').ABAPGitAgent;
    agent = new ABAPGitAgent();
  });

  describe('agent.list', () => {
    test('returns success=true with objects list', async () => {
      mockList.mockResolvedValue({
        success: 'X',
        package: '$ZTEST',
        objects: [
          { type: 'CLAS', name: 'ZCL_CLASS1' },
          { type: 'CLAS', name: 'ZCL_CLASS2' },
          { type: 'INTF', name: 'ZIF_INTERFACE1' }
        ],
        by_type: [
          { type: 'CLAS', count: 2 },
          { type: 'INTF', count: 1 }
        ],
        total: 3
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
      mockList.mockResolvedValue({
        success: 'X',
        package: '$ZEMPTY',
        objects: [],
        by_type: [],
        total: 0
      });

      const result = await agent.list('$ZEMPTY');

      expect(result.success).toBe(true);
      expect(result.objects).toEqual([]);
      expect(result.by_type).toEqual([]);
      expect(result.total).toBe(0);
    });

    test('filters by type when specified', async () => {
      mockList.mockResolvedValue({
        success: 'X',
        package: '$ZTEST',
        objects: [
          { type: 'CLAS', name: 'ZCL_CLASS1' },
          { type: 'CLAS', name: 'ZCL_CLASS2' }
        ],
        by_type: [{ type: 'CLAS', count: 2 }],
        total: 2
      });

      await agent.list('$ZTEST', 'CLAS');

      expect(mockList).toHaveBeenCalledWith('$ZTEST', 'CLAS', null, 100, 0);
    });

    test('filters by name pattern when specified', async () => {
      mockList.mockResolvedValue({
        success: 'X',
        package: '$ZTEST',
        objects: [{ type: 'CLAS', name: 'ZCL_TEST_CLASS' }],
        by_type: [{ type: 'CLAS', count: 1 }],
        total: 1
      });

      await agent.list('$ZTEST', null, '*TEST*');

      expect(mockList).toHaveBeenCalledWith('$ZTEST', null, '*TEST*', 100, 0);
    });

    test('handles custom limit and offset', async () => {
      mockList.mockResolvedValue({
        success: 'X',
        objects: [],
        total: 50
      });

      await agent.list('$ZTEST', null, null, 25, 10);

      expect(mockList).toHaveBeenCalledWith('$ZTEST', null, null, 25, 10);
    });

    test('returns success=false on error response', async () => {
      mockList.mockResolvedValue({
        success: '',
        error: 'Package not found',
        objects: [],
        by_type: [],
        total: 0
      });

      const result = await agent.list('$ZINVALID');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Package not found');
    });

    test('throws error on exception', async () => {
      mockList.mockRejectedValue(new Error('Network error'));

      await expect(agent.list('$ZTEST'))
        .rejects.toThrow('List command failed: Network error');
    });

    test('handles uppercase response keys from ABAP', async () => {
      mockList.mockResolvedValue({
        SUCCESS: 'X',
        PACKAGE: '$ZTEST',
        OBJECTS: [{ TYPE: 'CLAS', NAME: 'ZCL_CLASS1' }],
        BY_TYPE: [{ TYPE: 'CLAS', COUNT: 1 }],
        TOTAL: 1
      });

      const result = await agent.list('$ZTEST');

      expect(result.success).toBe(true);
      expect(result.package).toBe('$ZTEST');
      expect(result.objects).toHaveLength(1);
      expect(result.objects[0].type).toBe('CLAS');
      expect(result.objects[0].name).toBe('ZCL_CLASS1');
      expect(result.by_type).toHaveLength(1);
      expect(result.by_type[0].type).toBe('CLAS');
      expect(result.by_type[0].count).toBe(1);
      expect(result.total).toBe(1);
    });

    test('handles mixed case response keys', async () => {
      mockList.mockResolvedValue({
        SUCCESS: 'X',
        package: '$ZTEST',
        // Using PascalCase keys which should be handled
        objects: [{ TYPE: 'CLAS', NAME: 'ZCL_CLASS1' }],
        by_type: [{ TYPE: 'CLAS', COUNT: 1 }],
        total: 5
      });

      const result = await agent.list('$ZTEST');

      expect(result.success).toBe(true);
      expect(result.package).toBe('$ZTEST');
      // Should normalize all object properties to lowercase
      expect(result.objects).toHaveLength(1);
      expect(result.objects[0].type).toBe('CLAS');
      expect(result.objects[0].name).toBe('ZCL_CLASS1');
      expect(result.total).toBe(5);
    });

    test('handles missing response fields gracefully', async () => {
      mockList.mockResolvedValue({});

      const result = await agent.list('$ZTEST');

      expect(result.success).toBe(false);
      expect(result.command).toBe('LIST');
      expect(result.package).toBe('$ZTEST');
      expect(result.objects).toEqual([]);
      expect(result.by_type).toEqual([]);
      expect(result.total).toBe(0);
      expect(result.error).toBeNull();
    });

    test('handles null/undefined in response arrays', async () => {
      mockList.mockResolvedValue({
        success: 'X',
        objects: null,
        by_type: undefined,
        total: null
      });

      const result = await agent.list('$ZTEST');

      expect(result.success).toBe(true);
      expect(result.objects).toEqual([]);
      expect(result.by_type).toEqual([]);
      expect(result.total).toBe(0);
    });

    test('combines all filter parameters correctly', async () => {
      mockList.mockResolvedValue({
        success: 'X',
        objects: [{ type: 'CLAS', name: 'ZCL_TEST' }],
        total: 1
      });

      await agent.list('$ZTEST', 'CLAS,INTF', 'ZCL_*', 50, 20);

      expect(mockList).toHaveBeenCalledWith('$ZTEST', 'CLAS,INTF', 'ZCL_*', 50, 20);
    });

    test('uses default command when not in response', async () => {
      mockList.mockResolvedValue({
        success: 'X'
      });

      const result = await agent.list('$ZTEST');

      expect(result.command).toBe('LIST');
    });

    test('preserves command from response', async () => {
      mockList.mockResolvedValue({
        success: 'X',
        command: 'CUSTOM_LIST'
      });

      const result = await agent.list('$ZTEST');

      expect(result.command).toBe('CUSTOM_LIST');
    });

    test('handles multiple object types in filter', async () => {
      mockList.mockResolvedValue({
        success: 'X',
        objects: [
          { type: 'CLAS', name: 'ZCL_CLASS1' },
          { type: 'INTF', name: 'ZIF_INTF1' },
          { type: 'PROG', name: 'ZPROG1' }
        ],
        by_type: [
          { type: 'CLAS', count: 1 },
          { type: 'INTF', count: 1 },
          { type: 'PROG', count: 1 }
        ],
        total: 3
      });

      const result = await agent.list('$ZTEST', 'CLAS,INTF,PROG');

      expect(result.success).toBe(true);
      expect(result.objects).toHaveLength(3);
      expect(mockList).toHaveBeenCalledWith('$ZTEST', 'CLAS,INTF,PROG', null, 100, 0);
    });
  });
});
