/**
 * Unit tests for server.js
 */

const request = require('supertest');
const express = require('express');

// Mock agent before requiring server
jest.mock('../src/agent', () => ({
  ABAPGitAgent: jest.fn().mockImplementation(() => ({
    pull: jest.fn(),
    healthCheck: jest.fn()
  }))
}));

jest.mock('../src/config', () => ({
  getAgentConfig: jest.fn().mockReturnValue({ port: 3000 })
}));

describe('Server', () => {
  let app;
  let mockAgent;

  beforeEach(() => {
    jest.resetModules();
    const { ABAPGitAgent } = require('../src/agent');
    mockAgent = new ABAPGitAgent();
    require('../src/config').getAgentConfig.mockReturnValue({ port: 3000 });

    // Create express app similar to server
    app = express();
    app.use(express.json());

    app.get('/api/health', async (req, res) => {
      try {
        const health = await mockAgent.healthCheck();
        res.json(health);
      } catch (error) {
        res.status(503).json({ status: 'unhealthy', error: error.message });
      }
    });

    app.post('/api/pull', async (req, res) => {
      try {
        const { url, branch, username, password } = req.body;

        if (!url) {
          return res.status(400).json({
            success: false,
            error: 'Missing required parameter: url'
          });
        }

        const result = await mockAgent.pull(url, branch, username, password);
        res.json(result);
      } catch (error) {
        res.status(500).json({ success: false, error: error.message });
      }
    });
  });

  describe('GET /api/health', () => {
    test('returns healthy status', async () => {
      mockAgent.healthCheck.mockResolvedValue({
        status: 'healthy',
        abap: 'connected',
        version: '1.0.0'
      });

      const res = await request(app).get('/api/health');

      expect(res.status).toBe(200);
      expect(res.body.status).toBe('healthy');
    });

    test('returns unhealthy on error', async () => {
      mockAgent.healthCheck.mockRejectedValue(new Error('Connection failed'));

      const res = await request(app).get('/api/health');

      expect(res.status).toBe(503);
      expect(res.body.status).toBe('unhealthy');
    });
  });

  describe('POST /api/pull', () => {
    test('returns 400 when url is missing', async () => {
      const res = await request(app)
        .post('/api/pull')
        .send({});

      expect(res.status).toBe(400);
      expect(res.body.error).toContain('url');
    });

    test('returns success response', async () => {
      mockAgent.pull.mockResolvedValue({
        success: 'X',
        job_id: 'TEST123',
        message: 'Pull completed successfully',
        error_detail: null
      });

      const res = await request(app)
        .post('/api/pull')
        .send({ url: 'http://test.com/repo', branch: 'main' });

      expect(res.status).toBe(200);
      expect(res.body.success).toBe('X');
      expect(res.body.job_id).toBe('TEST123');
    });

    test('returns error response with error_detail', async () => {
      mockAgent.pull.mockResolvedValue({
        success: '',
        job_id: 'TEST456',
        message: 'Pull completed with errors',
        error_detail: 'Errors/Warnings:\n  - CLAS ZCL_TEST: Syntax error'
      });

      const res = await request(app)
        .post('/api/pull')
        .send({ url: 'http://test.com/repo' });

      expect(res.status).toBe(200);
      expect(res.body.success).toBe('');
      expect(res.body.error_detail).toContain('CLAS ZCL_TEST');
    });
  });
});
