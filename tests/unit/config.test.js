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

  describe('loadConfig - fallback to package directory', () => {
    test('reads config from CONFIG_PATH env var when set', () => {
      const fs = require('fs');
      process.env.CONFIG_PATH = '/custom/path/.abapGitAgent';
      // repo root check fails, CONFIG_PATH exists
      fs.existsSync.mockImplementation((p) => p === '/custom/path/.abapGitAgent');
      fs.readFileSync.mockReturnValue(JSON.stringify({ host: 'custom-host.com' }));

      const { loadConfig } = require('../../src/config');
      const cfg = loadConfig();
      expect(cfg.host).toBe('custom-host.com');
      delete process.env.CONFIG_PATH;
    });
  });

  describe('getTransport()', () => {
    test('returns transport from config file', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({
        host: 'test.com', user: 'u', password: 'p',
        transport: 'DEVK900001'
      }));
      const { getTransport } = require('../../src/config');
      expect(getTransport()).toBe('DEVK900001');
    });

    test('returns undefined when transport not configured', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({ host: 'test.com', user: 'u', password: 'p' }));
      const { getTransport } = require('../../src/config');
      expect(getTransport()).toBeUndefined();
    });
  });

  describe('isAbapIntegrationEnabled()', () => {
    test('returns true when .abapGitAgent exists in cwd', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({ host: 'x' }));
      const { isAbapIntegrationEnabled } = require('../../src/config');
      expect(isAbapIntegrationEnabled()).toBe(true);
    });

    test('returns false when .abapGitAgent does not exist', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(false);
      const { isAbapIntegrationEnabled } = require('../../src/config');
      expect(isAbapIntegrationEnabled()).toBe(false);
    });
  });

  describe('loadProjectConfig()', () => {
    test('returns parsed JSON when .abapgit-agent.json exists', () => {
      const fs = require('fs');
      // First call: .abapGitAgent (loadConfig); second: .abapgit-agent.json
      fs.existsSync.mockImplementation((p) => p.includes('abapgit-agent.json'));
      fs.readFileSync.mockReturnValue(JSON.stringify({ project: { name: 'MyProject' } }));
      const { loadProjectConfig } = require('../../src/config');
      const cfg = loadProjectConfig();
      expect(cfg.project.name).toBe('MyProject');
    });

    test('returns null when file not found', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(false);
      const { loadProjectConfig } = require('../../src/config');
      expect(loadProjectConfig()).toBeNull();
    });

    test('returns null and warns on invalid JSON', () => {
      const fs = require('fs');
      fs.existsSync.mockImplementation((p) => p.includes('abapgit-agent.json'));
      fs.readFileSync.mockReturnValue('{ invalid json }');
      const warns = [];
      const origWarn = console.warn;
      console.warn = (...a) => warns.push(a.join(' '));
      const { loadProjectConfig } = require('../../src/config');
      const result = loadProjectConfig();
      console.warn = origWarn;
      expect(result).toBeNull();
      expect(warns.join('\n')).toMatch(/Failed to parse/);
    });
  });

  describe('getSafeguards()', () => {
    test('returns defaults when no project config', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(false);
      const { getSafeguards } = require('../../src/config');
      const s = getSafeguards();
      expect(s.requireFilesForPull).toBe(false);
      expect(s.disablePull).toBe(false);
      expect(s.disableRun).toBe(false);
      expect(s.disableProbeClasses).toBe(false);
      expect(s.reason).toBeNull();
    });

    test('returns safeguards from project config', () => {
      const fs = require('fs');
      fs.existsSync.mockImplementation((p) => p.includes('abapgit-agent.json'));
      fs.readFileSync.mockReturnValue(JSON.stringify({
        safeguards: { requireFilesForPull: true, disablePull: true, reason: 'CI only' }
      }));
      const { getSafeguards } = require('../../src/config');
      const s = getSafeguards();
      expect(s.requireFilesForPull).toBe(true);
      expect(s.disablePull).toBe(true);
      expect(s.reason).toBe('CI only');
    });
  });

  describe('getProjectInfo()', () => {
    test('returns null when no project config', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(false);
      const { getProjectInfo } = require('../../src/config');
      expect(getProjectInfo()).toBeNull();
    });

    test('returns project info when present', () => {
      const fs = require('fs');
      fs.existsSync.mockImplementation((p) => p.includes('abapgit-agent.json'));
      fs.readFileSync.mockReturnValue(JSON.stringify({ project: { name: 'Foo', description: 'Bar' } }));
      const { getProjectInfo } = require('../../src/config');
      expect(getProjectInfo()).toEqual({ name: 'Foo', description: 'Bar' });
    });
  });

  describe('getConflictSettings()', () => {
    test('returns default abort mode when no project config', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(false);
      const { getConflictSettings } = require('../../src/config');
      expect(getConflictSettings()).toEqual({ mode: 'abort', reason: null });
    });

    test('returns configured mode and reason', () => {
      const fs = require('fs');
      fs.existsSync.mockImplementation((p) => p.includes('abapgit-agent.json'));
      fs.readFileSync.mockReturnValue(JSON.stringify({
        conflictDetection: { mode: 'ignore', reason: 'solo dev' }
      }));
      const { getConflictSettings } = require('../../src/config');
      expect(getConflictSettings()).toEqual({ mode: 'ignore', reason: 'solo dev' });
    });

    test('falls back to abort and warns on invalid mode', () => {
      const fs = require('fs');
      fs.existsSync.mockImplementation((p) => p.includes('abapgit-agent.json'));
      fs.readFileSync.mockReturnValue(JSON.stringify({
        conflictDetection: { mode: 'invalid-mode' }
      }));
      const warns = [];
      const origWarn = console.warn;
      console.warn = (...a) => warns.push(a.join(' '));
      const { getConflictSettings } = require('../../src/config');
      const result = getConflictSettings();
      console.warn = origWarn;
      expect(result.mode).toBe('abort');
      expect(warns.join('\n')).toMatch(/Invalid conflictDetection/);
    });
  });

  describe('getTransportHookConfig()', () => {
    test('returns null hook when no project config', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(false);
      const { getTransportHookConfig } = require('../../src/config');
      expect(getTransportHookConfig()).toEqual({ hook: null, description: null });
    });

    test('returns hook path and description from project config', () => {
      const fs = require('fs');
      fs.existsSync.mockImplementation((p) => p.includes('abapgit-agent.json'));
      fs.readFileSync.mockReturnValue(JSON.stringify({
        transports: { hook: { path: './scripts/get-transport.js', description: 'Earliest open TR' } }
      }));
      const { getTransportHookConfig } = require('../../src/config');
      expect(getTransportHookConfig()).toEqual({
        hook: './scripts/get-transport.js',
        description: 'Earliest open TR'
      });
    });
  });

  describe('getTransportSettings()', () => {
    test('returns defaults when no project config', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(false);
      const { getTransportSettings } = require('../../src/config');
      expect(getTransportSettings()).toEqual({ allowCreate: true, allowRelease: true, reason: null });
    });

    test('returns restricted settings from project config', () => {
      const fs = require('fs');
      fs.existsSync.mockImplementation((p) => p.includes('abapgit-agent.json'));
      fs.readFileSync.mockReturnValue(JSON.stringify({
        transports: { allowCreate: false, allowRelease: false, reason: 'Release manager only' }
      }));
      const { getTransportSettings } = require('../../src/config');
      expect(getTransportSettings()).toEqual({
        allowCreate: false,
        allowRelease: false,
        reason: 'Release manager only'
      });
    });
  });

  describe('getWorkflowConfig()', () => {
    test('returns trunk mode by default', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({ host: 'test.com', user: 'u', password: 'p' }));
      const { getWorkflowConfig } = require('../../src/config');
      expect(getWorkflowConfig()).toEqual({ mode: 'trunk', defaultBranch: null });
    });

    test('returns configured workflow mode and branch', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({
        host: 'test.com', user: 'u', password: 'p',
        workflow: { mode: 'branch', defaultBranch: 'main' }
      }));
      const { getWorkflowConfig } = require('../../src/config');
      expect(getWorkflowConfig()).toEqual({ mode: 'branch', defaultBranch: 'main' });
    });
  });
});
