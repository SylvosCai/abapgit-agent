'use strict';

const fs = require('fs');
const path = require('path');
const readline = require('readline');

// Marker present in the old full-guide CLAUDE.md (copied by init before this feature)
const FULL_GUIDE_MARKER = 'Claude Code Instructions';

// Marker present in the slim stub (so we can detect it's already migrated)
const SLIM_STUB_MARKER = 'abapgit-agent guide';

// Marker present in the old full copilot-instructions.md
const COPILOT_FULL_MARKER = '# ABAP Development with abapGit';

// Marker present in the slim copilot stub
const COPILOT_SLIM_MARKER = 'abapgit-agent guide';

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

  _getBundledGuidelineNames() {
    const candidates = [
      path.join(__dirname, '..', '..', 'abap', 'guidelines'),
      path.join(__dirname, '..', '..', '..', 'abap', 'guidelines')
    ];
    const guidelinesDir = candidates.find(p => fs.existsSync(p));
    if (!guidelinesDir) return new Set();
    return new Set(
      fs.readdirSync(guidelinesDir).filter(f => f.endsWith('.md'))
    );
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

    const bundledNames = this._getBundledGuidelineNames();
    const slimStubPath = this._findSlimStub();
    const copilotSlimStubPath = this._findCopilotSlimStub();

    // --- Scan guidelines/ ---
    const guidelinesDir = path.join(cwd, 'guidelines');
    const toDelete = [];    // standard files matching bundled names
    const toKeep = [];      // *.local.md or other non-standard files

    if (fs.existsSync(guidelinesDir)) {
      for (const name of fs.readdirSync(guidelinesDir)) {
        if (!name.endsWith('.md')) continue;
        if (bundledNames.has(name)) {
          toDelete.push(path.join(guidelinesDir, name));
        } else {
          toKeep.push(name);
        }
      }
    }

    // --- Scan CLAUDE.md ---
    const claudeMdPath = path.join(cwd, 'CLAUDE.md');
    let claudeMdAction = 'none'; // 'replace' | 'already-slim' | 'custom' | 'missing'
    if (fs.existsSync(claudeMdPath)) {
      const content = fs.readFileSync(claudeMdPath, 'utf8');
      if (content.includes(SLIM_STUB_MARKER)) {
        claudeMdAction = 'already-slim';
      } else if (content.includes(FULL_GUIDE_MARKER)) {
        claudeMdAction = 'replace';
      } else {
        claudeMdAction = 'custom';
      }
    } else {
      claudeMdAction = 'missing';
    }

    // --- Scan .github/copilot-instructions.md ---
    const copilotMdPath = path.join(cwd, '.github', 'copilot-instructions.md');
    let copilotAction = 'none'; // 'replace' | 'already-slim' | 'custom' | 'missing'
    if (fs.existsSync(copilotMdPath)) {
      const content = fs.readFileSync(copilotMdPath, 'utf8');
      if (content.includes(COPILOT_SLIM_MARKER)) {
        copilotAction = 'already-slim';
      } else if (content.includes(COPILOT_FULL_MARKER)) {
        copilotAction = 'replace';
      } else {
        copilotAction = 'custom';
      }
    } else {
      copilotAction = 'missing';
    }

    // --- Nothing to do? ---
    const nothingToDo = toDelete.length === 0 && claudeMdAction !== 'replace' && copilotAction !== 'replace';
    if (nothingToDo) {
      console.log('');
      console.log('✅ Already clean — nothing to migrate.');
      if (claudeMdAction === 'already-slim') {
        console.log('   CLAUDE.md is already the slim stub.');
      } else if (claudeMdAction === 'custom') {
        console.log('   CLAUDE.md has custom content — left untouched.');
      } else if (claudeMdAction === 'missing') {
        console.log('   No CLAUDE.md found.');
      }
      if (copilotAction === 'already-slim') {
        console.log('   .github/copilot-instructions.md is already the slim stub.');
      } else if (copilotAction === 'custom') {
        console.log('   .github/copilot-instructions.md has custom content — left untouched.');
      }
      if (toDelete.length === 0 && fs.existsSync(guidelinesDir)) {
        console.log('   No standard guideline files found in guidelines/.');
      }
      console.log('');
      return;
    }

    // --- Preview ---
    console.log('');
    console.log('🔄 guide --migrate: switch to bundled guidelines');
    console.log('');

    if (toDelete.length > 0) {
      console.log(`Files to remove (${toDelete.length} standard guideline file${toDelete.length > 1 ? 's' : ''}):`);
      toDelete.forEach(f => console.log(`   ${path.relative(cwd, f)}`));
      console.log('');
    }

    if (toKeep.length > 0) {
      console.log('Files to keep (project-specific):');
      toKeep.forEach(f => console.log(`   guidelines/${f}`));
      console.log('');
    }

    if (claudeMdAction === 'replace') {
      if (slimStubPath) {
        console.log('CLAUDE.md: detected as full guide → will replace with slim stub');
        console.log("   (run 'abapgit-agent guide' to read the full guide on demand)");
      } else {
        console.log('CLAUDE.md: detected as full guide → ⚠️  slim stub not found, will skip');
      }
      console.log('');
    }

    if (copilotAction === 'replace') {
      if (copilotSlimStubPath) {
        console.log('.github/copilot-instructions.md: detected as full guide → will replace with slim stub');
        console.log('   (Copilot uses the slim stub; full guide available online)');
      } else {
        console.log('.github/copilot-instructions.md: detected as full guide → ⚠️  slim stub not found, will skip');
      }
      console.log('');
    }

    // After deletions, would guidelines/ be empty?
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
    let deletedCount = 0;

    for (const filePath of toDelete) {
      fs.unlinkSync(filePath);
      console.log(`🗑️  Removed ${path.relative(cwd, filePath)}`);
      deletedCount++;
    }

    if (dirWillBeEmpty) {
      // Verify no other files remain before removing the directory
      const remaining = fs.readdirSync(guidelinesDir);
      if (remaining.length === 0) {
        fs.rmdirSync(guidelinesDir);
        console.log('🗑️  Removed guidelines/');
      }
    }

    if (claudeMdAction === 'replace' && slimStubPath) {
      const slimContent = fs.readFileSync(slimStubPath, 'utf8');
      fs.writeFileSync(claudeMdPath, slimContent);
      console.log('✅ Replaced CLAUDE.md with slim stub');
    }

    if (copilotAction === 'replace' && copilotSlimStubPath) {
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

    if (args.includes('--json')) {
      console.log(JSON.stringify({ path: filePath, content }));
      return;
    }

    console.log(content);
  }
};
