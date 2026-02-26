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

  describe('syntaxCheckSource', () => {
    it('calls AbapHttp.post with correct syntax check data', async () => {
      const objects = [{ type: 'CLAS', name: 'ZCL_TEST', source: 'CLASS zcl_test DEFINITION.' }];

      mockPost.mockResolvedValue({
        SUCCESS: true,
        MESSAGE: 'All checks passed',
        RESULTS: [{
          OBJECT_TYPE: 'CLAS',
          OBJECT_NAME: 'ZCL_TEST',
          SUCCESS: true,
          ERROR_COUNT: 0,
          ERRORS: [],
          WARNINGS: []
        }]
      });

      const result = await agent.syntaxCheckSource(objects, 'X');

      expect(mockPost).toHaveBeenCalledWith(
        '/sap/bc/z_abapgit_agent/syntax',
        expect.objectContaining({
          objects: objects,
          uccheck: 'X'
        }),
        { csrfToken: 'test-csrf-token' }
      );

      expect(result.success).toBe(true);
      expect(result.results).toHaveLength(1);
    });
  });

  describe('unitCheck', () => {
    it('calls AbapHttp.post for unit tests', async () => {
      mockPost.mockResolvedValue({
        SUCCESS: 'X',
        TEST_COUNT: 10,
        PASSED_COUNT: 10,
        FAILED_COUNT: 0,
        MESSAGE: 'All tests passed'
      });

      const result = await agent.unitCheck('$ZTEST_PACKAGE');

      expect(mockPost).toHaveBeenCalledWith(
        '/sap/bc/z_abapgit_agent/unit',
        expect.objectContaining({
          package: '$ZTEST_PACKAGE'
        }),
        { csrfToken: 'test-csrf-token' }
      );

      expect(result.success).toBe(true);
      expect(result.test_count).toBe(10);
    });
  });

  describe('create', () => {
    it('creates repository with correct parameters', async () => {
      mockPost.mockResolvedValue({
        SUCCESS: 'X',
        REPO_KEY: 'REPO123',
        REPO_NAME: 'test_repo',
        MESSAGE: 'Created successfully'
      });

      const result = await agent.create('https://github.com/test/repo.git', '$ZTEST', 'main');

      expect(mockPost).toHaveBeenCalledWith(
        '/sap/bc/z_abapgit_agent/create',
        expect.objectContaining({
          url: 'https://github.com/test/repo.git',
          package: '$ZTEST',
          branch: 'main'
        }),
        { csrfToken: 'test-csrf-token' }
      );

      expect(result.success).toBe(true);
      expect(result.repo_key).toBe('REPO123');
    });
  });

  describe('list', () => {
    it('lists objects with normalized keys', async () => {
      mockPost.mockResolvedValue({
        SUCCESS: 'X',
        OBJECTS: [
          { TYPE: 'CLAS', NAME: 'ZCL_TEST1' },
          { TYPE: 'INTF', NAME: 'ZIF_TEST1' }
        ],
        BY_TYPE: [
          { TYPE: 'CLAS', COUNT: 1 },
          { TYPE: 'INTF', COUNT: 1 }
        ],
        TOTAL: 2
      });

      const result = await agent.list('$ZTEST');

      expect(result.success).toBe(true);
      expect(result.objects).toHaveLength(2);
      expect(result.objects[0].type).toBe('CLAS');
      expect(result.by_type).toHaveLength(2);
    });
  });
});
