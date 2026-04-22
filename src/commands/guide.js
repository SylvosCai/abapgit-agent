'use strict';

const fs = require('fs');
const path = require('path');
const readline = require('readline');

module.exports = {
  name: 'guide',
  description: 'Show bundled ABAP development guide',
  requiresAbapConfig: false,

  _findBundledGuide() {
    const candidates = [
      path.join(__dirname, '..', '..', 'abap', 'CLAUDE.md'),
      path.join(__dirname, '..', '..', '..', 'abap', 'CLAUDE.md')
    ];
    return candidates.find(p => fs.existsSync(p)) || null;
  },

  _findSlimStub() {
    const candidates = [
      path.join(__dirname, '..', '..', 'abap', 'CLAUDE.slim.md'),
      path.join(__dirname, '..', '..', '..', 'abap', 'CLAUDE.slim.md')
    ];
    return candidates.find(p => fs.existsSync(p)) || null;
  },

  _findCopilotSlimStub() {
    const candidates = [
      path.join(__dirname, '..', '..', 'abap', '.github', 'copilot-instructions.slim.md'),
      path.join(__dirname, '..', '..', '..', 'abap', '.github', 'copilot-instructions.slim.md')
    ];
    return candidates.find(p => fs.existsSync(p)) || null;
  },

  async _confirm(question) {
    return new Promise((resolve) => {
      const rl = readline.createInterface({ input: process.stdin, output: process.stdout });
      rl.question(question, (answer) => {
        rl.close();
        const normalized = answer.trim().toLowerCase();
        resolve(normalized === '' || normalized === 'y' || normalized === 'yes');
      });
    });
  },

  async _runMigrate(args) {
    const dryRun = args.includes('--dry-run');
    const yes = args.includes('--yes') || args.includes('-y');
    const cwd = process.cwd();

    const slimStubPath = this._findSlimStub();
    const copilotSlimStubPath = this._findCopilotSlimStub();

    // --- Scan guidelines/: delete all *.md except *.local.md ---
    const guidelinesDir = path.join(cwd, 'guidelines');
    const toDelete = [];
    const toKeep = [];

    if (fs.existsSync(guidelinesDir)) {
      for (const name of fs.readdirSync(guidelinesDir)) {
        if (!name.endsWith('.md')) continue;
        if (name.endsWith('.local.md')) {
          toKeep.push(name);
        } else {
          toDelete.push(path.join(guidelinesDir, name));
        }
      }
    }

    // --- CLAUDE.md: replace if it exists ---
    const claudeMdPath = path.join(cwd, 'CLAUDE.md');
    const claudeExists = fs.existsSync(claudeMdPath);

    // --- .github/copilot-instructions.md: replace if it exists ---
    const copilotMdPath = path.join(cwd, '.github', 'copilot-instructions.md');
    const copilotExists = fs.existsSync(copilotMdPath);

    // --- Nothing to do? ---
    const nothingToDo = toDelete.length === 0 && !claudeExists && !copilotExists;
    if (nothingToDo) {
      console.log('');
      console.log('✅ Nothing to migrate — no guideline files, CLAUDE.md, or copilot-instructions.md found.');
      console.log('');
      return;
    }

    // --- Preview ---
    console.log('');
    console.log('🔄 guide --migrate: switch to bundled guidelines');
    console.log('');

    if (toDelete.length > 0) {
      console.log(`Files to remove (${toDelete.length} guideline file${toDelete.length > 1 ? 's' : ''}):`);
      toDelete.forEach(f => console.log(`   ${path.relative(cwd, f)}`));
      console.log('');
    }

    if (toKeep.length > 0) {
      console.log('Files to keep (project-specific):');
      toKeep.forEach(f => console.log(`   guidelines/${f}`));
      console.log('');
    }

    if (claudeExists) {
      if (slimStubPath) {
        console.log('CLAUDE.md → will replace with slim stub');
        console.log("   (run 'abapgit-agent guide' to read the full guide on demand)");
      } else {
        console.log('CLAUDE.md → ⚠️  slim stub not found, will skip');
      }
      console.log('');
    }

    if (copilotExists) {
      if (copilotSlimStubPath) {
        console.log('.github/copilot-instructions.md → will replace with slim stub');
      } else {
        console.log('.github/copilot-instructions.md → ⚠️  slim stub not found, will skip');
      }
      console.log('');
    }

    const dirWillBeEmpty = fs.existsSync(guidelinesDir) && toKeep.length === 0;
    if (dirWillBeEmpty) {
      console.log('guidelines/ will be removed (no project-specific files remain).');
      console.log('');
    }

    if (dryRun) {
      console.log('ℹ️  Dry run — no changes made.');
      console.log('');
      return;
    }

    // --- Confirm ---
    if (!yes) {
      const proceed = await this._confirm('Proceed? [y/N] ');
      if (!proceed) {
        console.log('Migration cancelled.');
        console.log('');
        return;
      }
    }

    // --- Execute ---
    console.log('');

    for (const filePath of toDelete) {
      fs.unlinkSync(filePath);
      console.log(`🗑️  Removed ${path.relative(cwd, filePath)}`);
    }

    if (dirWillBeEmpty) {
      const remaining = fs.readdirSync(guidelinesDir);
      if (remaining.length === 0) {
        fs.rmdirSync(guidelinesDir);
        console.log('🗑️  Removed guidelines/');
      }
    }

    if (claudeExists && slimStubPath) {
      const slimContent = fs.readFileSync(slimStubPath, 'utf8');
      fs.writeFileSync(claudeMdPath, slimContent);
      console.log('✅ Replaced CLAUDE.md with slim stub');
    }

    if (copilotExists && copilotSlimStubPath) {
      const slimContent = fs.readFileSync(copilotSlimStubPath, 'utf8');
      fs.writeFileSync(copilotMdPath, slimContent);
      console.log('✅ Replaced .github/copilot-instructions.md with slim stub');
    }

    console.log('');
    console.log('✅ Migration complete.');
    console.log('   Standard guidelines are now read from the package automatically.');
    console.log("   Run 'abapgit-agent ref \"<pattern>\"' or 'abapgit-agent ref --topic <topic>' to search them.");
    console.log('');
  },

  _extractAiContent(content) {
    const marker = '<!-- AI-CONDENSED-START -->';
    const markerIndex = content.indexOf(marker);
    if (markerIndex === -1) return content;
    return content.slice(markerIndex + marker.length).trimStart();
  },

  async execute(args) {
    if (args.includes('--migrate')) {
      return this._runMigrate(args);
    }

    const filePath = this._findBundledGuide();

    if (!filePath) {
      console.error('❌ Bundled CLAUDE.md not found. Make sure abapgit-agent is properly installed.');
      process.exit(1);
    }

    if (args.includes('--path')) {
      console.log(filePath);
      return;
    }

    const content = fs.readFileSync(filePath, 'utf8');

    if (args.includes('--ai')) {
      const aiContent = this._extractAiContent(content);
      if (args.includes('--json')) {
        console.log(JSON.stringify({ path: filePath, content: aiContent }));
        return;
      }
      console.log(aiContent);
      return;
    }

    if (args.includes('--json')) {
      console.log(JSON.stringify({ path: filePath, content }));
      return;
    }

    console.log(content);
  }
};
