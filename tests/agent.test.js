/**
 * Unit tests for agent.js
 */

// Create mock functions
const mockPull = jest.fn();
const mockHealthCheck = jest.fn();
const mockSyntaxCheck = jest.fn();
const mockUnitTest = jest.fn();

// Mock abap-client before requiring agent
jest.mock('../src/abap-client', () => ({
  getClient: jest.fn(() => ({
    pull: mockPull,
    healthCheck: mockHealthCheck,
    syntaxCheck: mockSyntaxCheck,
    unitTest: mockUnitTest
  }))
}));

// Clear module cache and require fresh
beforeEach(() => {
  jest.clearAllMocks();
});

describe('ABAPGitAgent', () => {
  let agent;
  let ABAPGitAgent;

  beforeEach(() => {
    jest.resetModules();
    // Re-setup mocks after reset
    jest.doMock('../src/abap-client', () => ({
      getClient: jest.fn(() => ({
        pull: mockPull,
        healthCheck: mockHealthCheck,
        syntaxCheck: mockSyntaxCheck,
        unitTest: mockUnitTest
      }))
    }));

    ABAPGitAgent = require('../src/agent').ABAPGitAgent;
    agent = new ABAPGitAgent();
  });

  describe('pull', () => {
    test('returns success=true for X response', async () => {
      mockPull.mockResolvedValue({
        success: 'X',
        job_id: 'TEST123',
        message: 'Pull completed successfully',
        error_detail: null
      });

      const result = await agent.pull('http://test.com/repo', 'main');

      expect(result.success).toBe(true);
      expect(result.job_id).toBe('TEST123');
      expect(result.message).toBe('Pull completed successfully');
      expect(result.error_detail).toBeNull();
    });

    test('returns success=true for boolean true response', async () => {
      mockPull.mockResolvedValue({
        success: true,
        job_id: 'TEST456',
        message: 'Success',
        error_detail: null
      });

      const result = await agent.pull('http://test.com/repo');

      expect(result.success).toBe(true);
    });

    test('returns success=false for error response', async () => {
      mockPull.mockResolvedValue({
        success: '',
        job_id: 'TEST789',
        message: 'Pull completed with errors',
        error_detail: 'Errors/Warnings:\n  - CLAS ZCL_TEST: Syntax error'
      });

      const result = await agent.pull('http://test.com/repo', 'main', 'user', 'pass');

      expect(result.success).toBe(false);
      expect(result.error_detail).toContain('CLAS ZCL_TEST');
    });

    test('throws error on exception', async () => {
      mockPull.mockRejectedValue(new Error('Network error'));

      await expect(agent.pull('http://test.com/repo'))
        .rejects.toThrow('Pull failed: Network error');
    });

    test('calls abap.pull with correct parameters', async () => {
      mockPull.mockResolvedValue({
        success: 'X',
        job_id: 'TEST',
        message: 'OK',
        error_detail: null
      });

      await agent.pull('http://test.com/repo', 'develop', 'user', 'pass');

      expect(mockPull).toHaveBeenCalledWith(
        'http://test.com/repo',
        'develop',
        'user',
        'pass',
        null  // files is optional
      );
    });

    test('handles uppercase response keys from ABAP', async () => {
      mockPull.mockResolvedValue({
        SUCCESS: 'X',
        JOB_ID: 'UPPER123',
        MESSAGE: 'Upper case response',
        ERROR_DETAIL: null,
        ACTIVATED_COUNT: 5,
        FAILED_COUNT: 0
      });

      const result = await agent.pull('http://test.com/repo');

      expect(result.success).toBe(true);
      expect(result.job_id).toBe('UPPER123');
      expect(result.activated_count).toBe(5);
    });
  });

  describe('healthCheck', () => {
    test('returns healthy status', async () => {
      mockHealthCheck.mockResolvedValue({
        status: 'OK',
        version: '1.0.0'
      });

      const result = await agent.healthCheck();

      expect(result.status).toBe('healthy');
      expect(result.abap).toBe('connected');
      expect(result.version).toBe('1.0.0');
    });

    test('returns unhealthy status on error', async () => {
      mockHealthCheck.mockRejectedValue(new Error('Connection refused'));

      const result = await agent.healthCheck();

      expect(result.status).toBe('unhealthy');
      expect(result.abap).toBe('disconnected');
    });
  });

  describe('syntaxCheck', () => {
    test('returns success=true for clean syntax check', async () => {
      mockSyntaxCheck.mockResolvedValue({
        success: 'X',
        object_type: 'CLAS',
        object_name: 'ZCL_TEST',
        error_count: 0,
        errors: []
      });

      const result = await agent.syntaxCheck('CLAS', 'ZCL_TEST');

      expect(result.success).toBe(true);
      expect(result.object_type).toBe('CLAS');
      expect(result.object_name).toBe('ZCL_TEST');
      expect(result.error_count).toBe(0);
      expect(result.errors).toEqual([]);
    });

    test('returns errors when syntax issues found', async () => {
      mockSyntaxCheck.mockResolvedValue({
        success: '',
        object_type: 'CLAS',
        object_name: 'ZCL_TEST',
        error_count: 2,
        errors: [
          { line: '15', column: '12', text: 'Variable "LV_TEST" not found' },
          { line: '20', column: '5', text: 'SYNTAX_ERROR' }
        ]
      });

      const result = await agent.syntaxCheck('CLAS', 'ZCL_TEST');

      expect(result.success).toBe(false);
      expect(result.error_count).toBe(2);
      expect(result.errors).toHaveLength(2);
    });

    test('throws error on exception', async () => {
      mockSyntaxCheck.mockRejectedValue(new Error('ABAP system error'));

      await expect(agent.syntaxCheck('CLAS', 'ZCL_TEST'))
        .rejects.toThrow('Syntax check failed: ABAP system error');
    });

    test('handles uppercase response keys', async () => {
      mockSyntaxCheck.mockResolvedValue({
        SUCCESS: 'X',
        OBJECT_TYPE: 'CLAS',
        OBJECT_NAME: 'ZCL_TEST',
        ERROR_COUNT: 1,
        ERRORS: [{ LINE: '10', COLUMN: '5', TEXT: 'Error' }]
      });

      const result = await agent.syntaxCheck('CLAS', 'ZCL_TEST');

      expect(result.success).toBe(true);
      expect(result.object_type).toBe('CLAS');
      expect(result.error_count).toBe(1);
    });
  });

  describe('unitCheck', () => {
    test('returns success=true for all tests passed', async () => {
      mockUnitTest.mockResolvedValue({
        success: 'X',
        test_count: 10,
        passed_count: 10,
        failed_count: 0,
        message: 'All 10 tests passed',
        errors: []
      });

      const result = await agent.unitCheck('ZTEST_PACKAGE');

      expect(result.success).toBe(true);
      expect(result.test_count).toBe(10);
      expect(result.passed_count).toBe(10);
      expect(result.failed_count).toBe(0);
      expect(result.errors).toEqual([]);
    });

    test('returns failure count when tests fail', async () => {
      mockUnitTest.mockResolvedValue({
        success: '',
        test_count: 5,
        passed_count: 3,
        failed_count: 2,
        message: '2 of 5 tests failed',
        errors: [
          { class_name: 'ZCL_TEST', method_name: 'TEST_1', error_kind: 'ERROR', error_text: 'Expected X but got Y' },
          { class_name: 'ZCL_TEST', method_name: 'TEST_2', error_kind: 'FAILURE', error_text: 'Reference is initial' }
        ]
      });

      const result = await agent.unitCheck('ZTEST_PACKAGE');

      expect(result.success).toBe(false);
      expect(result.test_count).toBe(5);
      expect(result.failed_count).toBe(2);
      expect(result.errors).toHaveLength(2);
      expect(result.errors[0].class_name).toBe('ZCL_TEST');
      expect(result.errors[0].method_name).toBe('TEST_1');
    });

    test('accepts objects parameter', async () => {
      mockUnitTest.mockResolvedValue({
        success: 'X',
        test_count: 2,
        passed_count: 2,
        failed_count: 0,
        errors: []
      });

      const objects = [
        { object_type: 'CLAS', object_name: 'ZCL_TEST1' },
        { object_type: 'CLAS', object_name: 'ZCL_TEST2' }
      ];

      await agent.unitCheck(null, objects);

      expect(mockUnitTest).toHaveBeenCalledWith(null, objects);
    });

    test('throws error on exception', async () => {
      mockUnitTest.mockRejectedValue(new Error('AUnit execution failed'));

      await expect(agent.unitCheck('ZTEST_PACKAGE'))
        .rejects.toThrow('Unit tests failed: AUnit execution failed');
    });

    test('handles uppercase response keys', async () => {
      mockUnitTest.mockResolvedValue({
        SUCCESS: 'X',
        TEST_COUNT: 5,
        PASSED_COUNT: 5,
        FAILED_COUNT: 0,
        MESSAGE: 'All passed',
        ERRORS: []
      });

      const result = await agent.unitCheck('ZTEST_PACKAGE');

      expect(result.success).toBe(true);
      expect(result.test_count).toBe(5);
      expect(result.passed_count).toBe(5);
    });
  });
});
