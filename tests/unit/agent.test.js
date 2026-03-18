/**
 * Unit tests for agent.js
 */

// Create mock functions for AbapHttp
const mockFetchCsrfToken = jest.fn();
const mockGet = jest.fn();
const mockPost = jest.fn();

// Mock AbapHttp
jest.mock('../../src/utils/abap-http', () => ({
  AbapHttp: jest.fn().mockImplementation(() => ({
    fetchCsrfToken: mockFetchCsrfToken,
    get: mockGet,
    post: mockPost
  }))
}));

// Mock config
jest.mock('../../src/config', () => ({
  getAbapConfig: jest.fn(() => ({
    host: 'test.example.com',
    sapport: 443,
    client: '100',
    user: 'TEST_USER',
    password: 'test',
    language: 'EN',
    gitUsername: 'git_user',
    gitPassword: 'git_token'
  }))
}));

// Clear module cache and require fresh
beforeEach(() => {
  jest.clearAllMocks();
  mockFetchCsrfToken.mockResolvedValue('test-csrf-token');
});

describe('ABAPGitAgent', () => {
  let agent;
  let ABAPGitAgent;

  beforeEach(() => {
    jest.resetModules();
    ABAPGitAgent = require('../../src/agent').ABAPGitAgent;
    agent = new ABAPGitAgent();
  });

  describe('constructor', () => {
    it('creates agent instance with AbapHttp client', () => {
      expect(agent).toBeDefined();
      expect(agent.http).toBeDefined();
    });
  });

  describe('pull', () => {
    it('calls AbapHttp.post with correct parameters', async () => {
      mockPost.mockResolvedValue({
        SUCCESS: 'X',
        JOB_ID: 'TEST123',
        MESSAGE: 'Pull completed',
        ACTIVATED_COUNT: 5,
        FAILED_COUNT: 0,
        ACTIVATED_OBJECTS: [],
        FAILED_OBJECTS: []
      });

      const result = await agent.pull('https://github.com/test/repo.git', 'main');

      expect(mockFetchCsrfToken).toHaveBeenCalled();
      expect(mockPost).toHaveBeenCalledWith(
        '/sap/bc/z_abapgit_agent/pull',
        expect.objectContaining({
          url: 'https://github.com/test/repo.git',
          branch: 'main'
        }),
        { csrfToken: 'test-csrf-token' }
      );

      expect(result.success).toBe(true);
      expect(result.job_id).toBe('TEST123');
    });

    it('handles errors gracefully', async () => {
      mockPost.mockRejectedValue(new Error('Connection failed'));

      await expect(agent.pull('https://github.com/test/repo.git')).rejects.toThrow('Pull failed');
    });
  });

  describe('healthCheck', () => {
    it('returns healthy status when ABAP is reachable', async () => {
      mockGet.mockResolvedValue({ version: '1.4.0' });

      const result = await agent.healthCheck();

      expect(result.status).toBe('healthy');
      expect(result.abap).toBe('connected');
      expect(result.version).toBe('1.4.0');
    });

    it('returns unhealthy status when ABAP is unreachable', async () => {
      mockGet.mockRejectedValue(new Error('Connection failed'));

      const result = await agent.healthCheck();

      expect(result.status).toBe('unhealthy');
      expect(result.abap).toBe('disconnected');
    });
  });
});
