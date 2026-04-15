/**
 * Init command - Initialize local repository configuration
 */

const pathModule = require('path');
const fs = require('fs');
const readline = require('readline');

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

/**
 * Detect old numbered guideline files (e.g. 00_index.md, 01_sql.md) and
 * prompt user to delete them if found.
 */
async function cleanupOldGuidelineFiles(guidelinesPath) {
  if (!fs.existsSync(guidelinesPath)) return;

  const oldFiles = fs.readdirSync(guidelinesPath)
    .filter(f => /^\d+_.*\.md$/.test(f));

  if (oldFiles.length === 0) return;

  console.log('');
  console.log('⚠️  Old numbered guideline files detected (from a previous version):');
  oldFiles.forEach(f => console.log(`   guidelines/${f}`));
  console.log('   These have been renamed and are no longer used.');

  const answer = await new Promise((resolve) => {
    const rl = readline.createInterface({ input: process.stdin, output: process.stdout });
    rl.question('Delete old files? [Y/n] ', (ans) => {
      rl.close();
      resolve(ans.trim().toLowerCase());
    });
  });

  if (answer === '' || answer === 'y' || answer === 'yes') {
    oldFiles.forEach(f => {
      fs.unlinkSync(pathModule.join(guidelinesPath, f));
      console.log(`   🗑  Deleted guidelines/${f}`);
    });
    console.log(`✅ Old guideline files removed`);
  } else {
    console.log('⚠️  Old files kept — they may appear as duplicates in ref search results');
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
  Creates .abapGitAgent, .abapgit-agent.json, .abapgit.xml, .gitignore, CLAUDE.md, and guidelines folder.

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

      // copilot-instructions.md: never overwrite — user may have customised it
      const localCopilotMdPath = pathModule.join(process.cwd(), '.github', 'copilot-instructions.md');
      if (!fs.existsSync(localCopilotMdPath)) {
        await copyFileIfExists(
          pathModule.join(__dirname, '..', '..', 'abap', '.github', 'copilot-instructions.slim.md'),
          localCopilotMdPath,
          '.github/copilot-instructions.md',
          true  // create parent dir
        );
      } else {
        console.log(`⚠️  .github/copilot-instructions.md already exists, skipped`);
      }

      // CLAUDE.md: never overwrite — user may have customised it
      const localClaudeMdPath = pathModule.join(process.cwd(), 'CLAUDE.md');
      if (!fs.existsSync(localClaudeMdPath)) {
        await copyFileIfExists(
          pathModule.join(__dirname, '..', '..', 'abap', 'CLAUDE.slim.md'),
          localClaudeMdPath,
          'CLAUDE.md'
        );
      } else {
        console.log(`⚠️  CLAUDE.md already exists, skipped (use 'abapgit-agent guide --migrate' to replace a full guide with the slim stub)`);
      }

      // guidelines/: never copy standard files — they're bundled in the package now.
      // Only ensure objects.local.md stub exists if missing.
      const guidelinesDestPath = pathModule.join(process.cwd(), 'guidelines');
      if (!fs.existsSync(guidelinesDestPath)) {
        fs.mkdirSync(guidelinesDestPath, { recursive: true });
        console.log(`✅ Created guidelines/`);
      }
      const localNamingPath = pathModule.join(guidelinesDestPath, 'objects.local.md');
      if (!fs.existsSync(localNamingPath)) {
        // reuse the same stub content as init
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
        console.log(`✅ Created guidelines/objects.local.md`);
      } else {
        console.log(`⚠️  guidelines/objects.local.md already exists, skipped`);
      }

      // Detect and offer to remove old numbered guideline files (legacy cleanup)
      await cleanupOldGuidelineFiles(guidelinesDestPath);

      console.log(`
📋 Update complete!
   Standard guidelines are read from the package automatically — no local copies needed.
   Run 'abapgit-agent guide --migrate' if you still have copied guideline files to remove.
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

    // Copy CLAUDE.md (slim stub — tells Claude to run 'abapgit-agent guide')
    const claudeMdPath = pathModule.join(__dirname, '..', '..', 'abap', 'CLAUDE.slim.md');
    const localClaudeMdPath = pathModule.join(process.cwd(), 'CLAUDE.md');
    try {
      if (fs.existsSync(localClaudeMdPath)) {
        console.log(`⚠️  CLAUDE.md already exists, skipped`);
      } else if (fs.existsSync(claudeMdPath)) {
        fs.copyFileSync(claudeMdPath, localClaudeMdPath);
        console.log(`✅ Created CLAUDE.md`);
      } else {
        console.log(`⚠️  CLAUDE.slim.md not found in abap/ directory`);
      }
    } catch (error) {
      console.error(`Error copying CLAUDE.md: ${error.message}`);
    }

    // Copy copilot-instructions.md for GitHub Copilot (slim stub)
    const copilotMdPath = pathModule.join(__dirname, '..', '..', 'abap', '.github', 'copilot-instructions.slim.md');
    const githubDir = pathModule.join(process.cwd(), '.github');
    const localCopilotMdPath = pathModule.join(githubDir, 'copilot-instructions.md');
    try {
      if (fs.existsSync(localCopilotMdPath)) {
        console.log(`⚠️  .github/copilot-instructions.md already exists, skipped`);
      } else if (fs.existsSync(copilotMdPath)) {
        if (!fs.existsSync(githubDir)) {
          fs.mkdirSync(githubDir, { recursive: true });
        }
        fs.copyFileSync(copilotMdPath, localCopilotMdPath);
        console.log(`✅ Created .github/copilot-instructions.md`);
      } else {
        console.log(`⚠️  copilot-instructions.slim.md not found in abap/.github/ directory`);
      }
    } catch (error) {
      console.error(`Error copying copilot-instructions.md: ${error.message}`);
    }

    // Create guidelines/ directory and objects.local.md stub
    // (Standard guidelines are bundled in the package — no need to copy them)
    const guidelinesDestPath = pathModule.join(process.cwd(), 'guidelines');
    try {
      if (!fs.existsSync(guidelinesDestPath)) {
        fs.mkdirSync(guidelinesDestPath, { recursive: true });
        console.log(`✅ Created guidelines/`);
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
    } catch (error) {
      console.error(`Error creating guidelines: ${error.message}`);
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

    // Create .abapgit.xml with correct STARTING_FOLDER so abapGit's remove_ignored_files()
    // keeps files inside the source folder. Without this file the stored starting_folder in
    // the ABAP persistence may not match the actual folder, causing all remote files to be
    // silently ignored and pull to return ACTIVATED_COUNT=0 with an empty log.
    const abapgitXmlPath = pathModule.join(process.cwd(), '.abapgit.xml');
    if (!fs.existsSync(abapgitXmlPath)) {
      const language = (config.language || 'E').toUpperCase().charAt(0);
      const abapgitXml = `<?xml version="1.0" encoding="utf-8"?>
<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
 <asx:values>
  <DATA>
   <MASTER_LANGUAGE>${language}</MASTER_LANGUAGE>
   <STARTING_FOLDER>${folder}</STARTING_FOLDER>
   <FOLDER_LOGIC>${folderLogic}</FOLDER_LOGIC>
  </DATA>
 </asx:values>
</asx:abap>
`;
      try {
        fs.writeFileSync(abapgitXmlPath, abapgitXml);
        console.log(`✅ Created .abapgit.xml (STARTING_FOLDER=${folder}, FOLDER_LOGIC=${folderLogic})`);
      } catch (error) {
        console.error(`Error creating .abapgit.xml: ${error.message}`);
      }
    } else {
      console.log(`✅ .abapgit.xml already exists, skipped`);
    }

    // Create .abapgit-agent.json with default values (team config, checked into git)
    const projectConfigPath = pathModule.join(process.cwd(), '.abapgit-agent.json');
    if (!fs.existsSync(projectConfigPath)) {
      const projectConfigSamplePath = pathModule.join(__dirname, '..', '..', '.abapgit-agent.example.json');
      if (!fs.existsSync(projectConfigSamplePath)) {
        console.error('Error: .abapgit-agent.example.json not found.');
        process.exit(1);
      }
      try {
        const projectConfig = JSON.parse(fs.readFileSync(projectConfigSamplePath, 'utf8'));
        projectConfig.project.name = packageName;
        fs.writeFileSync(projectConfigPath, JSON.stringify(projectConfig, null, 2) + '\n');
        console.log(`✅ Created .abapgit-agent.json (team config — commit this to git)`);
      } catch (error) {
        console.error(`Error creating .abapgit-agent.json: ${error.message}`);
      }
    } else {
      console.log(`✅ .abapgit-agent.json already exists, skipped`);
    }

    console.log(`
📋 Next steps:
   1. Edit .abapGitAgent with your ABAP credentials (host, user, password)
   2. Review .abapgit-agent.json and commit it to git (team settings)
   3. Run 'abapgit-agent create --import' to create online repository
   4. Run 'abapgit-agent pull' to activate objects

💡 Tips:
   • Only guidelines/objects.local.md needs to live in your repo.
     Standard guidelines are read from the package automatically via 'ref'.
   • Run 'abapgit-agent guide' to read the full ABAP development guide.
`);
  }
};
