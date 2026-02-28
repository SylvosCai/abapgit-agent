/**
 * Unit tests for workflow configuration
 */

// Mock modules
jest.mock('fs');
jest.mock('child_process');

describe('Workflow Configuration', () => {
  beforeEach(() => {
    jest.resetModules();
    jest.clearAllMocks();
  });

  describe('getWorkflowConfig', () => {
    it('should return default trunk mode when no config exists', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(false);

      const { getWorkflowConfig } = require('../../src/config');
      const config = getWorkflowConfig();

      expect(config).toEqual({
        mode: 'trunk',
        defaultBranch: null
      });
    });

    it('should return branch mode when configured', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({
        host: 'test.com',
        workflow: {
          mode: 'branch'
        }
      }));

      const { getWorkflowConfig } = require('../../src/config');
      const config = getWorkflowConfig();

      expect(config).toEqual({
        mode: 'branch',
        defaultBranch: null
      });
    });

    it('should return trunk mode when explicitly configured', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({
        host: 'test.com',
        workflow: {
          mode: 'trunk'
        }
      }));

      const { getWorkflowConfig } = require('../../src/config');
      const config = getWorkflowConfig();

      expect(config).toEqual({
        mode: 'trunk',
        defaultBranch: null
      });
    });

    it('should return configured defaultBranch when provided', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({
        host: 'test.com',
        workflow: {
          mode: 'branch',
          defaultBranch: 'develop'
        }
      }));

      const { getWorkflowConfig } = require('../../src/config');
      const config = getWorkflowConfig();

      expect(config).toEqual({
        mode: 'branch',
        defaultBranch: 'develop'
      });
    });

    it('should return trunk mode when workflow config is missing', () => {
      const fs = require('fs');
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue(JSON.stringify({
        host: 'test.com',
        client: '100'
      }));

      const { getWorkflowConfig } = require('../../src/config');
      const config = getWorkflowConfig();

      expect(config).toEqual({
        mode: 'trunk',
        defaultBranch: null
      });
    });
  });

  describe('getDefaultBranch', () => {
    it('should return configured branch when provided', () => {
      const { getDefaultBranch } = require('../../src/utils/git-utils');
      const branch = getDefaultBranch('develop');
      expect(branch).toBe('develop');
    });

    it('should detect main from remote HEAD', () => {
      const { execSync } = require('child_process');
      execSync.mockReturnValueOnce('refs/remotes/origin/main\n');

      const { getDefaultBranch } = require('../../src/utils/git-utils');
      const branch = getDefaultBranch();

      expect(branch).toBe('main');
      expect(execSync).toHaveBeenCalledWith(
        'git symbolic-ref refs/remotes/origin/HEAD 2>/dev/null',
        expect.any(Object)
      );
    });

    it('should detect master from remote HEAD', () => {
      const { execSync } = require('child_process');
      execSync.mockReturnValueOnce('refs/remotes/origin/master\n');

      const { getDefaultBranch } = require('../../src/utils/git-utils');
      const branch = getDefaultBranch();

      expect(branch).toBe('master');
    });

    it('should fallback to checking remote branches when HEAD fails', () => {
      const { execSync } = require('child_process');
      execSync
        .mockImplementationOnce(() => { throw new Error('No HEAD'); })
        .mockReturnValueOnce('  origin/main\n  origin/feature/test\n');

      const { getDefaultBranch } = require('../../src/utils/git-utils');
      const branch = getDefaultBranch();

      expect(branch).toBe('main');
    });

    it('should detect master from remote branches', () => {
      const { execSync } = require('child_process');
      execSync
        .mockImplementationOnce(() => { throw new Error('No HEAD'); })
        .mockReturnValueOnce('  origin/master\n  origin/feature/test\n');

      const { getDefaultBranch } = require('../../src/utils/git-utils');
      const branch = getDefaultBranch();

      expect(branch).toBe('master');
    });

    it('should detect develop from remote branches', () => {
      const { execSync } = require('child_process');
      execSync
        .mockImplementationOnce(() => { throw new Error('No HEAD'); })
        .mockReturnValueOnce('  origin/develop\n  origin/feature/test\n');

      const { getDefaultBranch } = require('../../src/utils/git-utils');
      const branch = getDefaultBranch();

      expect(branch).toBe('develop');
    });

    it('should prefer main over master when both exist', () => {
      const { execSync } = require('child_process');
      execSync
        .mockImplementationOnce(() => { throw new Error('No HEAD'); })
        .mockReturnValueOnce('  origin/main\n  origin/master\n');

      const { getDefaultBranch } = require('../../src/utils/git-utils');
      const branch = getDefaultBranch();

      expect(branch).toBe('main');
    });

    it('should fallback to main when all detection methods fail', () => {
      const { execSync } = require('child_process');
      execSync
        .mockImplementationOnce(() => { throw new Error('No HEAD'); })
        .mockImplementationOnce(() => { throw new Error('No branches'); });

      const { getDefaultBranch } = require('../../src/utils/git-utils');
      const branch = getDefaultBranch();

      expect(branch).toBe('main');
    });
  });
});
