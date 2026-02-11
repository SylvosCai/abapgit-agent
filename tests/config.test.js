/**
 * Unit tests for config.js
 */

// Mock fs module
jest.mock('fs');

describe('Config', () => {
  beforeEach(() => {
    jest.resetModules();
    // Clear environment variables
    delete process.env.ABAP_HOST;
    delete process.env.ABAP_PORT;
    delete process.env.ABAP_CLIENT;
    delete process.env.ABAP_USER;
    delete process.env.ABAP_PASSWORD;
    delete process.env.ABAP_LANGUAGE;
    delete process.env.AGENT_PORT;
  });

  test('loads config from .abapGitAgent file', () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue(JSON.stringify({
      host: 'test-host.com',
      sapport: 44300,
      client: '100',
      user: 'TEST_USER',
      password: 'test_pass',
      language: 'EN',
      agent: { port: 3000 }
    }));

    const { loadConfig } = require('../src/config');
    const config = loadConfig();

    expect(config.host).toBe('test-host.com');
    expect(config.sapport).toBe(44300);
    expect(config.client).toBe('100');
    expect(config.agent.port).toBe(3000);
  });

  test('falls back to environment variables', () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(false);

    process.env.ABAP_HOST = 'env-host.com';
    process.env.ABAP_PORT = '443';
    process.env.ABAP_CLIENT = '200';
    process.env.ABAP_USER = 'ENV_USER';
    process.env.ABAP_PASSWORD = 'env_pass';
    process.env.ABAP_LANGUAGE = 'DE';

    const { loadConfig } = require('../src/config');
    const config = loadConfig();

    expect(config.host).toBe('env-host.com');
    expect(config.sapport).toBe(443);
    expect(config.client).toBe('200');
    expect(config.user).toBe('ENV_USER');
  });

  test('getAbapConfig returns correct structure', () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue(JSON.stringify({
      host: 'test.com',
      sapport: 44300,
      client: '100',
      user: 'user',
      password: 'pass',
      language: 'EN',
      gitUsername: 'git-user',
      gitPassword: 'git-pass'
    }));

    const { getAbapConfig } = require('../src/config');
    const abapConfig = getAbapConfig();

    expect(abapConfig).toEqual({
      host: 'test.com',
      sapport: 44300,
      client: '100',
      user: 'user',
      password: 'pass',
      language: 'EN',
      gitUsername: 'git-user',
      gitPassword: 'git-pass'
    });
  });
});
