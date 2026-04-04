/**
 * Upgrade command - Upgrade CLI and/or ABAP backend to latest or specific version
 */

const { execSync } = require('child_process');
const readline = require('readline');

module.exports = {
  name: 'upgrade',
  description: 'Upgrade CLI and/or ABAP backend to latest or specific version',
  requiresAbapConfig: false,  // Checked conditionally
  requiresVersionCheck: false, // Does its own version checks

  async execute(args, context) {
    const { versionCheck, loadConfig, isAbapIntegrationEnabled } = context;

    if (args.includes('--help') || args.includes('-h')) {
      console.log(`
Usage:
  abapgit-agent upgrade [--abap-only] [--cli-only] [--version <v>] [--check] [--yes]

Description:
  Upgrade the abapgit-agent CLI (npm package) and/or the ABAP backend to the latest
  or a specific version.

Parameters:
  --abap-only        Upgrade only the ABAP backend (skip CLI npm upgrade).
  --cli-only         Upgrade only the CLI npm package (skip ABAP backend).
  --version <v>      Upgrade to a specific version (e.g. 1.15.0).
  --check            Check for available upgrades without installing.
  --transport <T>    Transport request for ABAP backend upgrade.
  --yes, -y          Skip confirmation prompt.
  --dry-run          Show what would be done without making changes.

Examples:
  abapgit-agent upgrade
  abapgit-agent upgrade --check
  abapgit-agent upgrade --cli-only
  abapgit-agent upgrade --abap-only --transport DEVK900001
  abapgit-agent upgrade --version 1.15.0
`);
      return;
    }

    // Parse flags
    const flags = this.parseFlags(args);

    // Validate flag combinations
    try {
      this.validateFlags(flags);
    } catch (error) {
      console.error(`❌ Error: ${error.message}`);
      process.exit(1);
    }

    // Check if ABAP config is needed
    const needsAbapConfig = !flags.cliOnly && !flags.checkOnly;
    if (needsAbapConfig && !isAbapIntegrationEnabled()) {
      console.error('❌ Error: .abapGitAgent config file not found');
      console.error('   ABAP upgrade requires configuration.');
      console.error('   Run: abapgit-agent init');
      console.error('');
      console.error('   Or use --cli-only to upgrade CLI package only.');
      process.exit(1);
    }

    // Get current versions
    const cliVersion = versionCheck.getCliVersion();
    let abapVersion = null;

    if (needsAbapConfig) {
      try {
        const config = loadConfig();
        const { apiVersion } = await versionCheck.checkCompatibility(config);
        abapVersion = apiVersion;
      } catch (e) {
        console.error(`⚠️  Could not reach ABAP system: ${e.message}`);
      }
    }

    // If ABAP is unreachable and we need to upgrade it, warn and ask whether to continue CLI-only
    if (needsAbapConfig && abapVersion === null && !flags.cliOnly && !flags.abapOnly && !flags.match) {
      console.error('');
      console.error('⚠️  ABAP system is unreachable. Cannot upgrade ABAP backend.');
      if (flags.yes) {
        console.log('   --yes flag set: upgrading CLI only.');
        flags.cliOnly = true;
      } else {
        const proceed = await this.confirmCliOnlyFallback();
        if (!proceed) {
          console.log('Upgrade cancelled.');
          return;
        }
        flags.cliOnly = true;
      }
      console.log('   Run "abapgit-agent upgrade --abap-only" once the system is back.');
      console.log('');
    }

    // Get latest version from npm
    const latestVersion = await versionCheck.getLatestNpmVersion();
    if (!latestVersion && !flags.version && !flags.match) {
      console.error('❌ Error: Could not fetch latest version from npm registry');
      console.error('   Please check your internet connection or specify --version X.X.X');
      process.exit(1);
    }

    // Validate specified version exists in npm registry
    if (flags.version && !flags.abapOnly) {
      const versionExists = await this.validateVersionExists(flags.version);
      if (!versionExists) {
        console.error(`❌ Error: Version ${flags.version} not found in npm registry`);
        console.error('   Please check available versions at: https://www.npmjs.com/package/abapgit-agent?activeTab=versions');
        process.exit(1);
      }
    }

    // Determine target versions
    const targets = this.determineTargets(flags, cliVersion, abapVersion, latestVersion);

    // Check-only mode
    if (flags.checkOnly) {
      this.showCheckReport(cliVersion, abapVersion, latestVersion);
      return;
    }

    // Dry-run mode
    if (flags.dryRun) {
      this.showDryRunPlan(cliVersion, abapVersion, targets, flags);
      return;
    }

    // Show upgrade plan
    if (!flags.yes) {
      const proceed = await this.confirmUpgrade(cliVersion, abapVersion, targets, flags);
      if (!proceed) {
        console.log('Upgrade cancelled.');
        return;
      }
    }

    // Execute upgrade
    await this.performUpgrade(targets, flags, context);

    // Verify upgrade
    await this.verifyUpgrade(targets, flags, context);
  },

  /**
   * Parse command-line flags
   */
  parseFlags(args) {
    return {
      checkOnly: args.includes('--check'),
      cliOnly: args.includes('--cli-only'),
      abapOnly: args.includes('--abap-only'),
      match: args.includes('--match'),
      version: this.getArgValue(args, '--version'),
      latest: args.includes('--latest'),
      yes: args.includes('--yes') || args.includes('-y'),
      dryRun: args.includes('--dry-run'),
      transport: this.getArgValue(args, '--transport')
    };
  },

  /**
   * Get argument value following a flag
   */
  getArgValue(args, flag) {
    const index = args.indexOf(flag);
    if (index !== -1 && index + 1 < args.length) {
      return args[index + 1];
    }
    return null;
  },

  /**
   * Validate flag combinations
   */
  validateFlags(flags) {
    // Invalid combinations
    if (flags.match && flags.version) {
      throw new Error('Cannot use --match and --version together');
    }

    if (flags.match && flags.cliOnly) {
      throw new Error('Cannot use --match with --cli-only. --match upgrades ABAP to match CLI version');
    }

    if (flags.cliOnly && flags.abapOnly) {
      throw new Error('Cannot use --cli-only and --abap-only together');
    }
  },

  /**
   * Validate that a version exists in npm registry
   */
  async validateVersionExists(version) {
    return new Promise((resolve) => {
      const { execSync } = require('child_process');
      try {
        // Use npm view to check if version exists
        const output = execSync(`npm view abapgit-agent@${version} version 2>/dev/null`, {
          encoding: 'utf8',
          stdio: ['pipe', 'pipe', 'ignore']
        }).trim();

        // If npm view returns the version, it exists
        resolve(output === version);
      } catch (e) {
        // Version doesn't exist or npm command failed
        resolve(false);
      }
    });
  },

  /**
   * Determine target versions for CLI and ABAP
   */
  determineTargets(flags, cliVersion, abapVersion, latestVersion) {
    let cliTarget = null;
    let abapTarget = null;

    if (flags.match) {
      // Match ABAP to CLI version
      abapTarget = cliVersion;
    } else if (flags.version) {
      // Specific version
      cliTarget = flags.cliOnly ? flags.version : (flags.abapOnly ? null : flags.version);
      abapTarget = flags.abapOnly ? flags.version : (flags.cliOnly ? null : flags.version);
    } else {
      // Latest version (default)
      cliTarget = flags.abapOnly ? null : latestVersion;
      abapTarget = flags.cliOnly ? null : latestVersion;
    }

    return { cliTarget, abapTarget };
  },

  /**
   * Show check-only report
   */
  showCheckReport(cliVersion, abapVersion, latestVersion) {
    console.log('');
    console.log('Current versions:');
    console.log(`  CLI:  v${cliVersion}`);
    if (abapVersion) {
      console.log(`  ABAP: v${abapVersion}`);
    }
    console.log('');

    if (latestVersion) {
      console.log(`Latest available: v${latestVersion}`);
      console.log('');
    }

    const needsCliUpgrade = latestVersion && cliVersion !== latestVersion;
    const needsAbapUpgrade = abapVersion && latestVersion && abapVersion !== latestVersion;
    const versionMismatch = abapVersion && cliVersion !== abapVersion;

    if (versionMismatch) {
      console.log('⚠️  Version mismatch detected');
      console.log('');
    } else if (!needsCliUpgrade && !needsAbapUpgrade) {
      console.log('✅ All components are up to date');
      console.log('');
      return;
    }

    console.log('To upgrade:');
    console.log('  Both:      abapgit-agent upgrade');
    console.log('  CLI only:  abapgit-agent upgrade --cli-only');
    if (abapVersion) {
      console.log('  ABAP only: abapgit-agent upgrade --abap-only');
      console.log('  Match:     abapgit-agent upgrade --match');
    }
    console.log('');
  },

  /**
   * Show dry-run plan
   */
  showDryRunPlan(cliVersion, abapVersion, targets, flags) {
    console.log('');
    console.log('🔹 DRY RUN - No changes will be made');
    console.log('');
    console.log('Current versions:');
    console.log(`  CLI:  v${cliVersion}`);
    if (abapVersion) {
      console.log(`  ABAP: v${abapVersion}`);
    }
    console.log('');

    console.log('Target versions:');
    if (targets.cliTarget) {
      console.log(`  CLI:  v${targets.cliTarget}`);
    }
    if (targets.abapTarget) {
      console.log(`  ABAP: v${targets.abapTarget}`);
    }
    console.log('');

    console.log('Would execute:');
    let step = 1;
    if (targets.cliTarget) {
      console.log(`  ${step}. npm install -g abapgit-agent@${targets.cliTarget}`);
      step++;
    }
    if (targets.abapTarget) {
      console.log(`  ${step}. abapgit-agent pull --url <agentRepoUrl> --branch v${targets.abapTarget}`);
      console.log('     (agentRepoUrl from .abapGitAgent config or current git remote)');
      step++;
    }
    console.log(`  ${step}. Verify versions match`);
    console.log('');
    console.log('No changes made.');
    console.log('');
  },

  /**
   * Confirm upgrade with user
   */
  async confirmUpgrade(cliVersion, abapVersion, targets, flags) {
    console.log('');
    console.log('📦 Upgrade Plan:');
    console.log('');
    console.log('Current versions:');
    console.log(`  CLI:  v${cliVersion}`);
    if (abapVersion) {
      console.log(`  ABAP: v${abapVersion}`);
    }
    console.log('');

    console.log('Target versions:');
    if (targets.cliTarget) {
      console.log(`  CLI:  v${targets.cliTarget}`);
    }
    if (targets.abapTarget) {
      console.log(`  ABAP: v${targets.abapTarget}`);
    }
    console.log('');

    console.log('This will:');
    let step = 1;
    if (targets.cliTarget) {
      console.log(`  ${step}. Upgrade npm package: abapgit-agent@${targets.cliTarget}`);
      step++;
    }
    if (targets.abapTarget) {
      console.log(`  ${step}. Pull ABAP code from git tag v${targets.abapTarget}`);
      step++;
      console.log(`  ${step}. Activate all backend components`);
      step++;
    }
    console.log('');

    return new Promise((resolve) => {
      const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
      });

      rl.question('Do you want to continue? [Y/n] ', (answer) => {
        rl.close();
        const normalized = answer.trim().toLowerCase();
        resolve(normalized === '' || normalized === 'y' || normalized === 'yes');
      });
    });
  },

  /**
   * Perform upgrade
   */
  async performUpgrade(targets, flags, context) {
    console.log('');
    console.log('🚀 Starting upgrade...');
    console.log('');

    // Upgrade CLI
    if (targets.cliTarget) {
      await this.upgradeCliPackage(targets.cliTarget);
    }

    // Upgrade ABAP
    if (targets.abapTarget) {
      await this.upgradeAbapBackend(targets.abapTarget, flags.transport, context);
    }
  },

  /**
   * Upgrade CLI package via npm
   */
  async upgradeCliPackage(version) {
    console.log(`📦 Upgrading CLI to v${version}...`);

    try {
      // Check if npm is available
      try {
        execSync('npm --version', { stdio: 'ignore' });
      } catch (e) {
        console.error('❌ Error: npm is not installed or not in PATH');
        console.error('   Please install Node.js and npm: https://nodejs.org/');
        process.exit(1);
      }

      // Run npm install
      const command = `npm install -g abapgit-agent@${version}`;
      console.log(`   Running: ${command}`);

      execSync(command, {
        stdio: 'inherit',
        encoding: 'utf8'
      });

      console.log(`✅ CLI upgraded to v${version}`);
      console.log('');
    } catch (error) {
      console.error(`❌ Failed to upgrade CLI: ${error.message}`);
      console.error('');
      console.error('This may be due to:');
      console.error('  - Version not found in npm registry');
      console.error('  - Permission issues (try with sudo)');
      console.error('  - Network connectivity issues');
      process.exit(1);
    }
  },

  /**
   * Confirm CLI-only fallback when ABAP system is unreachable
   */
  async confirmCliOnlyFallback() {
    return new Promise((resolve) => {
      const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
      });

      rl.question('   Continue with CLI upgrade only? [Y/n] ', (answer) => {
        rl.close();
        const normalized = answer.trim().toLowerCase();
        resolve(normalized === '' || normalized === 'y' || normalized === 'yes');
      });
    });
  },

  /**
   */
  async upgradeAbapBackend(version, transport, context) {
    console.log(`📦 Upgrading ABAP backend to v${version}...`);
    console.log(`   Using git tag: v${version}`);
    console.log('');

    try {
      const { loadConfig } = context;

      // Determine the abapgit-agent repository URL.
      // Priority:
      //   1. agentRepoUrl from .abapGitAgent config (for forks/mirrors)
      //   2. Default canonical repository
      const config = loadConfig();
      const agentRepoUrl = config.agentRepoUrl || 'https://github.com/SylvosCai/abapgit-agent.git';

      // Execute pull command — pass null credentials so the canonical
      // public GitHub repo is not sent the project's SAP internal git credentials.
      const pullCommand = require('./pull');
      const { loadConfig: lc, AbapHttp } = context;
      await pullCommand.pull(agentRepoUrl, `v${version}`, null, transport || null, lc, AbapHttp, false, null);

      console.log('');
      console.log(`✅ ABAP backend upgraded to v${version}`);
      console.log('');
    } catch (error) {
      console.error(`❌ Failed to upgrade ABAP backend: ${error.message}`);
      console.error('');
      console.error('This may be due to:');
      console.error('  - ABAP system is unavailable');
      console.error('  - Git tag not found in repository');
      console.error('  - ABAP activation errors');
      console.error('  - Connection issues with ABAP system');
      console.error('');
      console.error('To retry once the system is available:');
      console.error(`  abapgit-agent upgrade --abap-only --version ${version}`);
      process.exit(1);
    }
  },

  /**
   * Verify upgrade success
   */
  async verifyUpgrade(targets, flags, context) {
    console.log('🔍 Verifying upgrade...');
    console.log('');

    const { versionCheck, loadConfig } = context;

    // Check CLI version
    let cliVersion = null;
    if (targets.cliTarget) {
      cliVersion = versionCheck.getCliVersion();
      if (cliVersion === targets.cliTarget) {
        console.log(`✅ CLI version verified: v${cliVersion}`);
      } else {
        console.log(`⚠️  CLI version mismatch: expected v${targets.cliTarget}, got v${cliVersion}`);
        console.log('   You may need to restart your terminal or reinstall globally');
      }
    }

    // Check ABAP version
    if (targets.abapTarget && !flags.cliOnly) {
      try {
        const config = loadConfig();
        const { apiVersion: abapVersion } = await versionCheck.checkCompatibility(config);

        if (abapVersion === targets.abapTarget) {
          console.log(`✅ ABAP version verified: v${abapVersion}`);
        } else {
          console.log(`⚠️  ABAP version mismatch: expected v${targets.abapTarget}, got v${abapVersion}`);
          console.log('   Some components may have failed to activate');
        }
      } catch (e) {
        console.log(`⚠️  Could not verify ABAP version: ${e.message}`);
      }
    }

    console.log('');
    console.log('✅ Upgrade complete!');
    console.log('');
  }
};
