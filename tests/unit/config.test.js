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
    delete process.env.ABAP_PROTOCOL;
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

    const { loadConfig } = require('../../src/config');
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

    const { loadConfig } = require('../../src/config');
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

    const { getAbapConfig } = require('../../src/config');
    const abapConfig = getAbapConfig();

    expect(abapConfig).toEqual({
      host: 'test.com',
      sapport: 44300,
      client: '100',
      user: 'user',
      password: 'pass',
      language: 'EN',
      gitUsername: 'git-user',
      gitPassword: 'git-pass',
      protocol: 'https'
    });
  });

  test('getAgentConfig returns agent config', () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue(JSON.stringify({
      host: 'test.com',
      sapport: 44300,
      client: '100',
      user: 'user',
      password: 'pass',
      agent: { port: 3000, host: 'localhost' }
    }));

    const { getAgentConfig } = require('../../src/config');
    const agentConfig = getAgentConfig();

    expect(agentConfig).toEqual({ port: 3000, host: 'localhost' });
  });

  test('getAgentConfig returns undefined when agent config not present', () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue(JSON.stringify({
      host: 'test.com',
      user: 'user',
      password: 'pass'
    }));

    const { getAgentConfig } = require('../../src/config');
    const agentConfig = getAgentConfig();

    expect(agentConfig).toBeUndefined();
  });

  test('getAbapConfig returns protocol from config file', () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue(JSON.stringify({
      host: 'test.com',
      sapport: 8000,
      client: '100',
      user: 'user',
      password: 'pass',
      protocol: 'http'
    }));

    const { getAbapConfig } = require('../../src/config');
    const abapConfig = getAbapConfig();

    expect(abapConfig.protocol).toBe('http');
  });

  test('getAbapConfig reads protocol from ABAP_PROTOCOL env var', () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(false);

    process.env.ABAP_HOST = 'env-host.com';
    process.env.ABAP_PROTOCOL = 'http';

    const { getAbapConfig } = require('../../src/config');
    const abapConfig = getAbapConfig();

    expect(abapConfig.protocol).toBe('http');
  });

  test('getAbapConfig defaults protocol to https', () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue(JSON.stringify({
      host: 'test.com',
      sapport: 443,
      client: '100',
      user: 'user',
      password: 'pass'
    }));

    const { getAbapConfig } = require('../../src/config');
    const abapConfig = getAbapConfig();

    expect(abapConfig.protocol).toBe('https');
  });

  describe('getScratchWorkspace', () => {
    test('returns null when not configured', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({
        host: 'test.com', user: 'CAIS', password: 'pass'
      }));

      const { getScratchWorkspace } = require('../../src/config');
      expect(getScratchWorkspace()).toBeNull();
    });

    test('derives classPrefix and programPrefix from user field', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({
        host: 'test.com', user: 'CAIS', password: 'pass',
        scratchWorkspace: { path: '/home/cais/scratch' }
      }));

      const { getScratchWorkspace } = require('../../src/config');
      const ws = getScratchWorkspace();

      expect(ws.path).toBe('/home/cais/scratch');
      expect(ws.classPrefix).toBe('ZCL_CAIS_');
      expect(ws.programPrefix).toBe('ZCAIS_');
    });

    test('uses explicit classPrefix and programPrefix when provided', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({
        host: 'test.com', user: 'CAIS', password: 'pass',
        scratchWorkspace: {
          path: '/home/cais/scratch',
          classPrefix: 'YCL_PROBE_',
          programPrefix: 'YPROBE_'
        }
      }));

      const { getScratchWorkspace } = require('../../src/config');
      const ws = getScratchWorkspace();

      expect(ws.classPrefix).toBe('YCL_PROBE_');
      expect(ws.programPrefix).toBe('YPROBE_');
    });

    test('falls back to PROBE prefix when user field is absent', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({
        host: 'test.com', password: 'pass',
        scratchWorkspace: { path: '/home/scratch' }
      }));

      const { getScratchWorkspace } = require('../../src/config');
      const ws = getScratchWorkspace();

      expect(ws.classPrefix).toBe('ZCL_PROBE_');
      expect(ws.programPrefix).toBe('ZPROBE_');
    });

    test('derives prefix from lowercased user correctly', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({
        host: 'test.com', user: 'john', password: 'pass',
        scratchWorkspace: { path: '/home/john/scratch' }
      }));

      const { getScratchWorkspace } = require('../../src/config');
      const ws = getScratchWorkspace();

      expect(ws.classPrefix).toBe('ZCL_JOHN_');
      expect(ws.programPrefix).toBe('ZJOHN_');
    });
  });
});
