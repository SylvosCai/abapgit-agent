/**
 * Unit tests for list command
 * Tests both ABAPClient and ABAPGitAgent list functionality
 */

// Mock fs module
const mockExistsSync = jest.fn();
const mockReadFileSync = jest.fn();
const mockWriteFileSync = jest.fn();
const mockUnlinkSync = jest.fn();

jest.mock('fs', () => ({
  existsSync: mockExistsSync,
  readFileSync: mockReadFileSync,
  writeFileSync: mockWriteFileSync,
  unlinkSync: mockUnlinkSync
}));

// Mock https and http
jest.mock('https', () => ({
  Agent: jest.fn(),
  request: jest.fn()
}));

jest.mock('http', () => ({
  request: jest.fn()
}));

// Mock logger
jest.mock('../../src/logger', () => ({
  info: jest.fn(),
  error: jest.fn(),
  warn: jest.fn(),
  debug: jest.fn()
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

// Create mock for list
const mockList = jest.fn();

// Clear module cache before tests
beforeEach(() => {
  jest.clearAllMocks();
});

describe('List Command - ABAPClient', () => {
  let ABAPClient;
  let client;

  beforeEach(() => {
    jest.resetModules();
    jest.clearAllMocks();

    ABAPClient = require('../../src/abap-client').ABAPClient;
    client = new ABAPClient();
  });

  test('list method exists and is callable', () => {
    expect(typeof client.list).toBe('function');
  });

  test('list sends correct request data with all parameters', async () => {
    client.request = jest.fn().mockResolvedValue({
      success: 'X',
      objects: [{ type: 'CLAS', name: 'ZCL_TEST' }],
      total: 1
    });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST', 'CLAS,INTF', 'ZCL_*', 50, 10);

    expect(client.request).toHaveBeenCalled();
    const callArgs = client.request.mock.calls[0];
    expect(callArgs[0]).toBe('POST');
    expect(callArgs[1]).toBe('/list');
    expect(callArgs[2].package).toBe('$ZTEST');
    expect(callArgs[2].type).toBe('CLAS,INTF');
    expect(callArgs[2].name).toBe('ZCL_*');
    expect(callArgs[2].limit).toBe(50);
    expect(callArgs[2].offset).toBe(10);
  });

  test('list sends correct request with only package', async () => {
    client.request = jest.fn().mockResolvedValue({
      success: 'X',
      objects: []
    });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST');

    expect(client.request).toHaveBeenCalled();
    const callArgs = client.request.mock.calls[0];
    expect(callArgs[2].package).toBe('$ZTEST');
    expect(callArgs[2].limit).toBe(100);
    expect(callArgs[2].offset).toBe(0);
    expect(callArgs[2].type).toBeUndefined();
    expect(callArgs[2].name).toBeUndefined();
  });

  test('list clamps limit to valid range', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST', null, null, 2000);
    await client.list('$ZTEST', null, null, 0);
    await client.list('$ZTEST', null, null, -5);

    // First call should clamp to 1000
    let callArgs = client.request.mock.calls[0];
    expect(callArgs[2].limit).toBe(1000);

    // Second call should clamp to 1
    callArgs = client.request.mock.calls[1];
    expect(callArgs[2].limit).toBe(1);

    // Third call should clamp to 1 (negative)
    callArgs = client.request.mock.calls[2];
    expect(callArgs[2].limit).toBe(1);
  });

  test('list clamps offset to non-negative', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST', null, null, 100, -10);

    const callArgs = client.request.mock.calls[0];
    expect(callArgs[2].offset).toBe(0);
  });

  test('list does not include type when null', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST', null);

    const callArgs = client.request.mock.calls[0];
    expect(callArgs[2].type).toBeUndefined();
  });

  test('list does not include name when null', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST', null, null);

    const callArgs = client.request.mock.calls[0];
    expect(callArgs[2].name).toBeUndefined();
  });

  test('list fetches CSRF token before making request', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST');

    expect(client.fetchCsrfToken).toHaveBeenCalled();
    expect(client.request).toHaveBeenCalled();
  });

  test('list passes CSRF token to request', async () => {
    // Mock both fetchCsrfToken and request to verify token is passed
    client.fetchCsrfToken = jest.fn().mockImplementation(async function() {
      this.csrfToken = 'my-csrf-token';
      return 'my-csrf-token';
    });
    client.request = jest.fn().mockResolvedValue({ success: 'X' });

    await client.list('$ZTEST');

    const callArgs = client.request.mock.calls[0];
    expect(callArgs[3]).toEqual({ csrfToken: 'my-csrf-token' });
  });

  test('list handles complex type filter', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST', 'CLAS,INTF,PROG,FUGR,DDLS');

    const callArgs = client.request.mock.calls[0];
    expect(callArgs[2].type).toBe('CLAS,INTF,PROG,FUGR,DDLS');
  });

  test('list handles wildcard name pattern', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST', null, 'ZCL_ABAPGIT*');

    const callArgs = client.request.mock.calls[0];
    expect(callArgs[2].name).toBe('ZCL_ABAPGIT*');
  });

  test('list handles empty string type as null', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST', '');

    const callArgs = client.request.mock.calls[0];
    // Empty string is falsy, so it won't be included
    expect(callArgs[2].type).toBeUndefined();
  });

  test('list handles empty string name as null', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST', null, '');

    const callArgs = client.request.mock.calls[0];
    // Empty string is falsy, so it won't be included
    expect(callArgs[2].name).toBeUndefined();
  });

  test('list uses default values when parameters omitted', async () => {
    client.request = jest.fn().mockResolvedValue({ success: 'X' });
    client.fetchCsrfToken = jest.fn().mockResolvedValue('test-csrf-token');

    await client.list('$ZTEST');

    const callArgs = client.request.mock.calls[0];
    expect(callArgs[2].package).toBe('$ZTEST');
    expect(callArgs[2].limit).toBe(100);
    expect(callArgs[2].offset).toBe(0);
  });
});

describe('List Command - ABAPGitAgent', () => {
  let agent;
  let ABAPGitAgent;

  beforeEach(() => {
    jest.resetModules();
    jest.clearAllMocks();
    // Re-setup mocks after reset
    jest.doMock('../../src/abap-client', () => ({
      getClient: jest.fn(() => ({
        list: mockList
      }))
    }));

    ABAPGitAgent = require('../../src/agent').ABAPGitAgent;
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
