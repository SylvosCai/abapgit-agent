/**
 * Unit tests for agent.js
 */

// Create mock functions
const mockPull = jest.fn();
const mockHealthCheck = jest.fn();

// Mock abap-client before requiring agent
jest.mock('../src/abap-client', () => ({
  getClient: jest.fn(() => ({
    pull: mockPull,
    healthCheck: mockHealthCheck
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
        healthCheck: mockHealthCheck
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
        'pass'
      );
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
});
