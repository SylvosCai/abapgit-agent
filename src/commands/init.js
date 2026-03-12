/**
 * Init command - Initialize local repository configuration
 */

const pathModule = require('path');
const fs = require('fs');

/**
 * Copy a file if source exists (helper for init --update)
 */
async function copyFileIfExists(srcPath, destPath, label, createParentDir = false) {
  try {
    if (fs.existsSync(srcPath)) {
      if (createParentDir) {
        const parentDir = pathModule.dirname(destPath);
        if (!fs.existsSync(parentDir)) {
          fs.mkdirSync(parentDir, { recursive: true });
        }
      }
      fs.copyFileSync(srcPath, destPath);
      console.log(`✅ Updated ${label}`);
    } else {
      console.log(`⚠️  ${label} not found in abapgit-agent`);
    }
  } catch (error) {
    console.error(`Error copying ${label}: ${error.message}`);
  }
}

/**
 * Copy guidelines folder (helper for init --update)
 */
async function copyGuidelinesFolder(srcPath, destPath, overwrite = false) {
  try {
    if (fs.existsSync(srcPath)) {
      // Create destination directory if needed
      if (!fs.existsSync(destPath)) {
        fs.mkdirSync(destPath, { recursive: true });
      }

      const files = fs.readdirSync(srcPath);
      let copied = 0;

      for (const file of files) {
        if (file.endsWith('.md')) {
          // Never overwrite *.local.md files — these are project-specific customisations
          if (file.endsWith('.local.md')) {
            continue;
          }

          const srcFile = pathModule.join(srcPath, file);
          const destFile = pathModule.join(destPath, file);

          // Skip if file exists and not overwriting
          if (fs.existsSync(destFile) && !overwrite) {
            continue;
          }

          fs.copyFileSync(srcFile, destFile);
          copied++;
        }
      }

      if (copied > 0) {
        console.log(`✅ Updated guidelines/ (${copied} files)`);
      } else {
        console.log(`⚠️  No guideline files found`);
      }
    } else {
      console.log(`⚠️  guidelines folder not found in abapgit-agent`);
    }
  } catch (error) {
    console.error(`Error copying guidelines: ${error.message}`);
  }
}

module.exports = {
  name: 'init',
  description: 'Initialize local repository configuration',
  requiresAbapConfig: false,
  requiresVersionCheck: false,

  async execute(args, context) {
    const { gitUtils } = context;

    // Show help if requested
    const helpIndex = args.findIndex(a => a === '--help' || a === '-h');
    if (helpIndex !== -1) {
      console.log(`
Usage:
  abapgit-agent init [options]

Description:
  Initialize local repository configuration.
  Creates .abapGitAgent, .gitignore, CLAUDE.md, and guidelines folder.

Options:
  --package <PACKAGE>             ABAP package name (required)
  --folder <FOLDER>               Starting folder (default: /src/)
  --folder-logic <PREFIX|FULL>    Folder logic (default: PREFIX)
                                  PREFIX: Subpackages derive folder names from parent prefix
                                  FULL: Use full package name as folder name
  --update                        Update CLAUDE.md and guidelines to latest version

Examples:
  abapgit-agent init --package \$MY_PACKAGE                    # Use defaults (/src/, PREFIX)
  abapgit-agent init --package \$MY_PACKAGE --folder /abap/    # Custom folder
  abapgit-agent init --package \$MY_PACKAGE --folder-logic FULL  # Custom folder logic
  abapgit-agent init --update                                  # Update files only
`);
      return;
    }

    const folderArgIndex = args.indexOf('--folder');
    const packageArgIndex = args.indexOf('--package');
    const folderLogicArgIndex = args.indexOf('--folder-logic');
    const updateMode = args.includes('--update');

    // Get parameters
    let folder = folderArgIndex !== -1 && folderArgIndex + 1 < args.length
      ? args[folderArgIndex + 1]
      : '/src/';

    // Normalize folder path: ensure it starts with / and ends with /
    folder = folder.trim();
    if (!folder.startsWith('/')) {
      folder = '/' + folder;
    }
    if (!folder.endsWith('/')) {
      folder = folder + '/';
    }

    const packageName = packageArgIndex !== -1 && packageArgIndex + 1 < args.length
      ? args[packageArgIndex + 1]
      : null;

    let folderLogic = folderLogicArgIndex !== -1 && folderLogicArgIndex + 1 < args.length
      ? args[folderLogicArgIndex + 1]
      : 'PREFIX';

    // Validate folder logic
    if (folderLogic !== 'PREFIX' && folderLogic !== 'FULL') {
      console.error('Error: --folder-logic must be either PREFIX or FULL');
      process.exit(1);
    }

    // Validate package is required for non-update mode
    if (!updateMode && !packageName) {
      console.error('Error: --package is required');
      console.error('Usage: abapgit-agent init --folder /src --package ZMY_PACKAGE');
      console.error('       abapgit-agent init --update    # Update files to latest version');
      process.exit(1);
    }

    // Check if current folder is git repo root
    const gitDir = pathModule.join(process.cwd(), '.git');
    if (!fs.existsSync(gitDir)) {
      console.error('Error: Current folder is not a git repository.');
      console.error('Run this command from the root folder of your git repository.');
      process.exit(1);
    }

    // In update mode, just copy the files and return
    if (updateMode) {
      console.log(`\n🔄 Updating abapGit Agent files`);
      console.log('');

      // Copy CLAUDE.md
      await copyFileIfExists(
        pathModule.join(__dirname, '..', '..', 'abap', 'CLAUDE.md'),
        pathModule.join(process.cwd(), 'CLAUDE.md'),
        'CLAUDE.md'
      );

      // Copy copilot-instructions.md
      await copyFileIfExists(
        pathModule.join(__dirname, '..', '..', 'abap', '.github', 'copilot-instructions.md'),
        pathModule.join(process.cwd(), '.github', 'copilot-instructions.md'),
        '.github/copilot-instructions.md',
        true  // create parent dir
      );

      // Copy guidelines folder to project root
      await copyGuidelinesFolder(
        pathModule.join(__dirname, '..', '..', 'abap', 'guidelines'),
        pathModule.join(process.cwd(), 'guidelines'),
        true  // overwrite
      );

      console.log(`
📋 Update complete!
   Run 'abapgit-agent ref --list-topics' to see available topics.
`);
      return;
    }

    // Validate package is required
    if (!packageName) {
      console.error('Error: --package is required');
      console.error('Usage: abapgit-agent init --folder /src --package ZMY_PACKAGE');
      console.error('       abapgit-agent init --update    # Update files to latest version');
      process.exit(1);
    }

    console.log(`\n🚀 Initializing abapGit Agent for local repository`);
    console.log(`   Folder: ${folder}`);
    console.log(`   Folder Logic: ${folderLogic}`);
    console.log(`   Package: ${packageName}`);
    console.log('');

    // Detect git remote URL
    const gitUrl = gitUtils.getRemoteUrl();
    if (!gitUrl) {
      console.error('Error: No git remote configured.');
      console.error('Configure a remote with: git remote add origin <url>');
      process.exit(1);
    }
    console.log(`📌 Git remote: ${gitUrl}`);

    // Check if .abapGitAgent already exists - merge if it does
    const configPath = pathModule.join(process.cwd(), '.abapGitAgent');
    let config = null;
    let isUpdate = false;

    if (fs.existsSync(configPath)) {
      isUpdate = true;
      try {
        // Read existing configuration
        const currentConfig = JSON.parse(fs.readFileSync(configPath, 'utf8'));

        console.log('📝 Updating existing .abapGitAgent configuration');
        console.log('');
        console.log('Current values:');
        console.log(`   Package:      ${currentConfig.package || '(not set)'}`);
        console.log(`   Folder:       ${currentConfig.folder || '(not set)'}`);
        console.log(`   Folder Logic: ${currentConfig.folderLogic || '(not set)'}`);
        console.log(`   Host:         ${currentConfig.host || '(not set)'}`);
        console.log('');

        // Merge: keep existing values, update package, folder, and folderLogic
        config = currentConfig;

        // Track what changed
        const changes = [];

        if (packageName && packageName !== currentConfig.package) {
          const oldValue = currentConfig.package || '(not set)';
          config.package = packageName;
          changes.push(`package: ${oldValue} → ${packageName}`);
        }

        if (folder && folder !== currentConfig.folder) {
          const oldValue = currentConfig.folder || '(not set)';
          config.folder = folder;
          changes.push(`folder: ${oldValue} → ${folder}`);
        }

        if (folderLogic && folderLogic !== currentConfig.folderLogic) {
          const oldValue = currentConfig.folderLogic || '(not set)';
          config.folderLogic = folderLogic;
          changes.push(`folderLogic: ${oldValue} → ${folderLogic}`);
        }

        if (changes.length === 0) {
          console.log('⚠️  No changes needed - package, folder, and folderLogic are already set correctly');
          console.log('');
          console.log('To change other settings, edit .abapGitAgent manually');
          // Don't exit - continue with other setup tasks (CLAUDE.md, guidelines, etc.)
        } else {
          console.log('Changes to be made:');
          changes.forEach(change => console.log(`   ${change}`));
          console.log('');
          console.log('✅ Keeping all other settings (host, credentials, workflow, etc.)');
          console.log('');
        }
      } catch (error) {
        console.error('Error: .abapGitAgent exists but could not read it:');
        console.error(`   ${error.message}`);
        console.error('');
        console.error('To fix:');
        console.error('   1. Check if .abapGitAgent contains valid JSON');
        console.error('   2. Or delete it: rm .abapGitAgent');
        process.exit(1);
      }
    } else {
      // Create new config from template
      const samplePath = pathModule.join(__dirname, '..', '..', '.abapGitAgent.example');
      if (!fs.existsSync(samplePath)) {
        console.error('Error: .abapGitAgent.example not found.');
        process.exit(1);
      }

      try {
        // Read sample and update with package/folder/folderLogic
        const sampleContent = fs.readFileSync(samplePath, 'utf8');
        config = JSON.parse(sampleContent);
        config.package = packageName;
        config.folder = folder;
        config.folderLogic = folderLogic;
      } catch (error) {
        console.error(`Error reading .abapGitAgent.example: ${error.message}`);
        process.exit(1);
      }
    }

    // Write the config (either new or updated)
    try {
      fs.writeFileSync(configPath, JSON.stringify(config, null, 2) + '\n');
      if (isUpdate) {
        console.log(`✅ Updated .abapGitAgent`);
      } else {
        console.log(`✅ Created .abapGitAgent`);
      }
    } catch (error) {
      console.error(`Error writing .abapGitAgent: ${error.message}`);
      process.exit(1);
    }

    // Update .gitignore
    const gitignorePath = pathModule.join(process.cwd(), '.gitignore');
    const ignoreEntries = ['.abapGitAgent'];
    let existingIgnore = '';

    if (fs.existsSync(gitignorePath)) {
      existingIgnore = fs.readFileSync(gitignorePath, 'utf8');
    }

    let updated = false;
    let newIgnoreContent = existingIgnore;

    for (const entry of ignoreEntries) {
      if (!newIgnoreContent.includes(entry)) {
        if (newIgnoreContent && !newIgnoreContent.endsWith('\n')) {
          newIgnoreContent += '\n';
        }
        newIgnoreContent += entry + '\n';
        updated = true;
      }
    }

    if (updated) {
      fs.writeFileSync(gitignorePath, newIgnoreContent);
      console.log(`✅ Updated .gitignore`);
    } else {
      fs.writeFileSync(gitignorePath, newIgnoreContent);
      console.log(`✅ .gitignore already up to date`);
    }

    // Copy CLAUDE.md
    const claudeMdPath = pathModule.join(__dirname, '..', '..', 'abap', 'CLAUDE.md');
    const localClaudeMdPath = pathModule.join(process.cwd(), 'CLAUDE.md');
    try {
      if (fs.existsSync(claudeMdPath)) {
        fs.copyFileSync(claudeMdPath, localClaudeMdPath);
        console.log(`✅ Created CLAUDE.md`);
      } else {
        console.log(`⚠️  CLAUDE.md not found in abap/ directory`);
      }
    } catch (error) {
      console.error(`Error copying CLAUDE.md: ${error.message}`);
    }

    // Copy copilot-instructions.md for GitHub Copilot
    const copilotMdPath = pathModule.join(__dirname, '..', '..', 'abap', '.github', 'copilot-instructions.md');
    const githubDir = pathModule.join(process.cwd(), '.github');
    const localCopilotMdPath = pathModule.join(githubDir, 'copilot-instructions.md');
    try {
      if (fs.existsSync(copilotMdPath)) {
        // Ensure .github directory exists
        if (!fs.existsSync(githubDir)) {
          fs.mkdirSync(githubDir, { recursive: true });
        }
        fs.copyFileSync(copilotMdPath, localCopilotMdPath);
        console.log(`✅ Created .github/copilot-instructions.md`);
      } else {
        console.log(`⚠️  copilot-instructions.md not found in abap/ directory`);
      }
    } catch (error) {
      console.error(`Error copying copilot-instructions.md: ${error.message}`);
    }

    // Copy guidelines folder to project root
    const guidelinesSrcPath = pathModule.join(__dirname, '..', '..', 'abap', 'guidelines');
    const guidelinesDestPath = pathModule.join(process.cwd(), 'guidelines');
    try {
      if (fs.existsSync(guidelinesSrcPath)) {
        if (!fs.existsSync(guidelinesDestPath)) {
          // Create guidelines directory
          fs.mkdirSync(guidelinesDestPath, { recursive: true });
          // Copy all files from guidelines folder
          const files = fs.readdirSync(guidelinesSrcPath);
          for (const file of files) {
            if (file.endsWith('.md')) {
              fs.copyFileSync(
                pathModule.join(guidelinesSrcPath, file),
                pathModule.join(guidelinesDestPath, file)
              );
            }
          }
          console.log(`✅ Created guidelines/ (${files.filter(f => f.endsWith('.md')).length} files)`);
        } else {
          console.log(`⚠️  guidelines/ already exists, skipped`);
        }

        // Create objects.local.md stub if not already present
        const localNamingPath = pathModule.join(guidelinesDestPath, 'objects.local.md');
        if (!fs.existsSync(localNamingPath)) {
          const localNamingStub = `---
nav_order: 8
---

# Project Naming Conventions (Override)

This file overrides \`guidelines/objects.md\` for this project.
It is **never overwritten** by \`abapgit-agent init --update\` — safe to customise.

Searched by the \`ref\` command alongside all other guidelines.

## Naming Conventions

Uncomment and edit the rows that differ from the defaults in \`guidelines/objects.md\`:

| Object Type | Prefix | Example |
|---|---|---|
| Class | ZCL_ | ZCL_MY_CLASS |
| Interface | ZIF_ | ZIF_MY_INTERFACE |
| Program | Z | ZMY_PROGRAM |
| Package | $ | $MY_PACKAGE |
| Table | Z | ZMY_TABLE |
| CDS View | ZC_ | ZC_MY_VIEW |
| CDS Entity | ZE_ | ZE_MY_ENTITY |
| Data Element | Z | ZMY_ELEMENT |
| Structure | Z | ZMY_STRUCTURE |
| Table Type | Z | ZMY_TABLE_TYPE |
`;
          fs.writeFileSync(localNamingPath, localNamingStub);
          console.log(`✅ Created guidelines/objects.local.md (project naming conventions)`);
        }
      } else {
        console.log(`⚠️  guidelines folder not found in abap/ directory`);
      }
    } catch (error) {
      console.error(`Error copying guidelines: ${error.message}`);
    }

    // Create folder
    const folderPath = pathModule.join(process.cwd(), folder);
    try {
      if (!fs.existsSync(folderPath)) {
        fs.mkdirSync(folderPath, { recursive: true });
        console.log(`✅ Created folder: ${folder}`);

        // Create .gitkeep
        const gitkeepPath = pathModule.join(folderPath, '.gitkeep');
        fs.writeFileSync(gitkeepPath, '');
      } else {
        console.log(`✅ Folder already exists: ${folder}`);
      }
    } catch (error) {
      console.error(`Error creating folder: ${error.message}`);
    }

    console.log(`
📋 Next steps:
   1. Edit .abapGitAgent with your ABAP credentials (host, user, password)
   2. Run 'abapgit-agent create --import' to create online repository
   3. Run 'abapgit-agent pull' to activate objects
`);
  }
};
