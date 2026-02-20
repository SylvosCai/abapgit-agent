/**
 * ABAP Reference Search - Search ABAP reference repositories for patterns
 *
 * This module provides portable reference lookup across multiple ABAP repositories
 * including cheat sheets and any other ABAP code repositories in the reference folder.
 */

const fs = require('fs');
const path = require('path');
const { promisify } = require('util');
const readdir = promisify(fs.readdir);
const readFile = promisify(fs.readFile);
const stat = promisify(fs.stat);

// Topic to file mapping
const TOPIC_MAP = {
  'internal-tables': '01_Internal_Tables.md',
  'structures': '02_Structures.md',
  'sql': '03_ABAP_SQL.md',
  'oop': '04_ABAP_Object_Orientation.md',
  'objects': '04_ABAP_Object_Orientation.md',
  'constructors': '05_Constructor_Expressions.md',
  'constructor': '05_Constructor_Expressions.md',
  'dynamic': '06_Dynamic_Programming.md',
  'rtti': '06_Dynamic_Programming.md',
  'strings': '07_String_Processing.md',
  'string': '07_String_Processing.md',
  'eml': '08_EML_ABAP_for_RAP.md',
  'hierarchies': '10_ABAP_SQL_Hierarchies.md',
  'grouping': '11_Internal_Tables_Grouping.md',
  'amdp': '12_AMDP.md',
  'flow': '13_Program_Flow_Logic.md',
  'unit-tests': '14_ABAP_Unit_Tests.md',
  'unit': '14_ABAP_Unit_Tests.md',
  'testing': '14_ABAP_Unit_Tests.md',
  'cds': '15_CDS_View_Entities.md',
  'datatypes': '16_Data_Types_and_Objects.md',
  'luw': '17_SAP_LUW.md',
  'dynpro': '18_Dynpro.md',
  'cloud': '19_ABAP_for_Cloud_Development.md',
  'selection-screens': '20_Selection_Screens_Lists.md',
  'json-xml': '21_XML_JSON.md',
  'json': '21_XML_JSON.md',
  'xml': '21_XML_JSON.md',
  'released-classes': '22_Released_ABAP_Classes.md',
  'datetime': '23_Date_and_Time.md',
  'functions': '24_Builtin_Functions.md',
  'auth': '25_Authorization_Checks.md',
  'authorization': '25_Authorization_Checks.md',
  'dictionary': '26_ABAP_Dictionary.md',
  'exceptions': '27_Exceptions.md',
  'exception': '27_Exceptions.md',
  'regex': '28_Regular_Expressions.md',
  'numeric': '29_Numeric_Operations.md',
  'ai': '30_Generative_AI.md',
  'where': '31_WHERE_Conditions.md',
  'performance': '32_Performance_Notes.md',
  'news': '33_ABAP_Release_News.md',
  'patterns': '34_OO_Design_Patterns.md',
  'design-patterns': '34_OO_Design_Patterns.md',
  'badis': '35_BAdIs.md',
  'rap': '36_RAP_Behavior_Definition_Language.md',
  'tables': '01_Internal_Tables.md'
};

/**
 * Detect reference folder from config or common locations
 * @returns {string|null} Path to reference folder or null if not found
 */
function detectReferenceFolder() {
  // Try config file in current working directory
  const configPath = path.join(process.cwd(), '.abapGitAgent');
  if (fs.existsSync(configPath)) {
    try {
      const config = JSON.parse(fs.readFileSync(configPath, 'utf8'));
      if (config.referenceFolder && fs.existsSync(config.referenceFolder)) {
        const cheatSheetsPath = path.join(config.referenceFolder, 'abap-cheat-sheets');
        if (fs.existsSync(cheatSheetsPath)) {
          return config.referenceFolder;
        }
      }
    } catch (e) {
      // Config exists but couldn't parse, continue to fallback
    }
  }

  // Fallback to common locations
  const homeDir = require('os').homedir();
  const commonPaths = [
    path.join(homeDir, 'abap-reference'),
    path.join(homeDir, 'Documents', 'abap-reference'),
    path.join(homeDir, 'Documents', 'code', 'abap-reference')
  ];

  for (const basePath of commonPaths) {
    const cheatSheetsPath = path.join(basePath, 'abap-cheat-sheets');
    if (fs.existsSync(cheatSheetsPath)) {
      return basePath;
    }
  }

  return null;
}

/**
 * Get cheat sheets directory path
 * @returns {string|null}
 */
function getCheatSheetsDir() {
  const refFolder = detectReferenceFolder();
  if (!refFolder) return null;
  return path.join(refFolder, 'abap-cheat-sheets');
}

/**
 * Get all ABAP repositories in the reference folder
 * @returns {Promise<Array<{name: string, path: string}>>}
 */
async function getReferenceRepositories() {
  const refFolder = detectReferenceFolder();
  if (!refFolder) return [];

  const repos = [];

  try {
    const entries = await readdir(refFolder);

    for (const entry of entries) {
      const fullPath = path.join(refFolder, entry);
      const stats = await stat(fullPath);

      // Check if it's a directory
      if (stats.isDirectory()) {
        // Check for custom-guidelines folder
        if (entry === 'custom-guidelines') {
          repos.push({
            name: entry,
            path: fullPath,
            isGitRepo: false,
            isCustomGuidelines: true
          });
          continue;
        }

        const gitDir = path.join(fullPath, '.git');
        const hasGit = fs.existsSync(gitDir);

        // Check for ABAP files or common ABAP project files
        const hasAbapFiles = await hasAbapContent(fullPath);

        if (hasGit || hasAbapFiles) {
          repos.push({
            name: entry,
            path: fullPath,
            isGitRepo: hasGit
          });
        }
      }
    }
  } catch (error) {
    // Return empty array on error
  }

  return repos.sort((a, b) => a.name.localeCompare(b.name));
}

/**
 * Check if a directory contains ABAP-related content
 * @param {string} dirPath
 * @returns {Promise<boolean>}
 */
async function hasAbapContent(dirPath) {
  const abapIndicators = [
    'abap/', 'src/', 'zcl_', 'zif_',
    '.abapgit.xml', 'package.devc.xml',
    '.clas.abap', '.intf.abap', '.tabl.xml'
  ];

  try {
    const entries = await readdir(dirPath);

    for (const entry of entries) {
      const lowerEntry = entry.toLowerCase();

      // Check for ABAP file extensions
      if (lowerEntry.endsWith('.abap') ||
          lowerEntry.endsWith('.clas.xml') ||
          lowerEntry.endsWith('.intf.xml') ||
          lowerEntry.endsWith('.tabl.xml') ||
          lowerEntry.endsWith('.ddls.asddls') ||
          lowerEntry.includes('zcl_') ||
          lowerEntry.includes('zif_')) {
        return true;
      }

      // Check for abap folder
      if (lowerEntry === 'abap' || lowerEntry === 'src') {
        return true;
      }
    }
  } catch (error) {
    return false;
  }

  return false;
}

/**
 * Recursively get all searchable files from a repository
 * @param {string} repoPath
 * @param {string} repoName
 * @param {Array<string>} extensions
 * @returns {Promise<Array<{repo: string, path: string, relativePath: string}>>}
 */
async function getSearchableFiles(repoPath, repoName, extensions = ['.md', '.abap', '.txt', '.asddls']) {
  const files = [];

  async function walkDir(currentPath, relativePath = '') {
    try {
      const entries = await readdir(currentPath);

      for (const entry of entries) {
        const fullPath = path.join(currentPath, entry);
        const relPath = path.join(relativePath, entry);
        const stats = await stat(fullPath);

        if (stats.isDirectory()) {
          // Skip common non-source directories
          const skipDirs = ['.git', 'node_modules', 'bin', 'tests', 'test'];
          if (!skipDirs.includes(entry.toLowerCase())) {
            await walkDir(fullPath, relPath);
          }
        } else if (stats.isFile()) {
          const ext = path.extname(entry).toLowerCase();
          const lowerEntry = entry.toLowerCase();

          // Check if file matches searchable extensions
          const isSearchable = extensions.includes(ext) ||
                              extensions.some(e => lowerEntry.endsWith(e));

          if (isSearchable) {
            files.push({
              repo: repoName,
              path: fullPath,
              relativePath: relPath
            });
          }
        }
      }
    } catch (error) {
      // Skip directories we can't read
    }
  }

  await walkDir(repoPath);
  return files;
}

/**
 * Search for a pattern across all reference repositories
 * @param {string} pattern - Pattern to search for
 * @returns {Promise<Object>} Search results
 */
async function searchPattern(pattern) {
  const refFolder = detectReferenceFolder();
  const repos = await getReferenceRepositories();

  if (!refFolder) {
    return {
      error: 'Reference folder not found',
      hint: 'Configure referenceFolder in .abapGitAgent or clone to ~/abap-reference'
    };
  }

  if (repos.length === 0) {
    return {
      error: 'No ABAP repositories found in reference folder',
      hint: 'Clone ABAP repositories to the reference folder to enable searching'
    };
  }

  const results = {
    pattern,
    referenceFolder: refFolder,
    repositories: repos.map(r => r.name),
    files: [],
    matches: []
  };

  try {
    // Search across all repositories
    for (const repo of repos) {
      const searchableFiles = await getSearchableFiles(repo.path, repo.name);

      for (const fileInfo of searchableFiles) {
        try {
          const content = await readFile(fileInfo.path, 'utf8');

          if (content.toLowerCase().includes(pattern.toLowerCase())) {
            results.files.push({
              repo: repo.name,
              file: fileInfo.relativePath
            });

            // Find matching lines with context
            const lines = content.split('\n');
            let matchCount = 0;

            for (let i = 0; i < lines.length; i++) {
              if (lines[i].toLowerCase().includes(pattern.toLowerCase())) {
                const start = Math.max(0, i - 1);
                const end = Math.min(lines.length, i + 2);
                const context = lines.slice(start, end).join('\n');

                results.matches.push({
                  repo: repo.name,
                  file: fileInfo.relativePath,
                  line: i + 1,
                  context
                });

                matchCount++;

                // Limit matches per file to avoid overwhelming output
                if (matchCount >= 3) {
                  break;
                }
              }
            }
          }
        } catch (error) {
          // Skip files we can't read
        }
      }
    }

    return results;
  } catch (error) {
    return {
      error: `Search failed: ${error.message}`
    };
  }
}

/**
 * Get content for a specific topic
 * @param {string} topic - Topic name
 * @returns {Promise<Object>} Topic content
 */
async function getTopic(topic) {
  const cheatSheetsDir = getCheatSheetsDir();

  if (!cheatSheetsDir) {
    return {
      error: 'Reference folder not found',
      hint: 'Configure referenceFolder in .abapGitAgent or clone to ~/abap-reference'
    };
  }

  const fileName = TOPIC_MAP[topic.toLowerCase()];
  if (!fileName) {
    return {
      error: `Unknown topic: ${topic}`,
      availableTopics: Object.keys(TOPIC_MAP).filter((v, i, a) => a.indexOf(v) === i).slice(0, 20)
    };
  }

  const filePath = path.join(cheatSheetsDir, fileName);

  if (!fs.existsSync(filePath)) {
    return {
      error: `File not found: ${fileName}`
    };
  }

  try {
    const content = await readFile(filePath, 'utf8');
    return {
      topic,
      file: fileName,
      content: content.slice(0, 5000), // First 5000 chars
      truncated: content.length > 5000,
      totalLength: content.length
    };
  } catch (error) {
    return {
      error: `Failed to read topic: ${error.message}`
    };
  }
}

/**
 * List available topics
 * @returns {Promise<Object>} List of topics
 */
async function listTopics() {
  const cheatSheetsDir = getCheatSheetsDir();

  if (!cheatSheetsDir) {
    return {
      error: 'Reference folder not found',
      hint: 'Configure referenceFolder in .abapGitAgent or clone to ~/abap-reference'
    };
  }

  // Build topic list from files that exist
  const topics = [];
  const seenFiles = new Set();

  for (const [topic, file] of Object.entries(TOPIC_MAP)) {
    if (!seenFiles.has(file) && fs.existsSync(path.join(cheatSheetsDir, file))) {
      topics.push({ topic, file });
      seenFiles.add(file);
    }
  }

  return {
    referenceFolder: path.dirname(cheatSheetsDir),
    topics: topics.sort((a, b) => a.file.localeCompare(b.file))
  };
}

/**
 * Display search results in console format
 * @param {Object} results - Search results
 */
function displaySearchResults(results) {
  if (results.error) {
    console.error(`\n  ❌ ${results.error}`);
    if (results.hint) {
      console.error(`\n  💡 ${results.hint}`);
    }
    return;
  }

  console.log(`\n  🔍 Searching for: '${results.pattern}'`);
  console.log(`  📁 Reference folder: ${results.referenceFolder}`);

  if (results.repositories && results.repositories.length > 0) {
    console.log(`  📚 Repositories (${results.repositories.length}): ${results.repositories.join(', ')}`);
  }
  console.log('');

  if (results.files.length === 0) {
    console.log('  ⚠️  No matches found.');
    return;
  }

  // Group files by repository
  const filesByRepo = {};
  results.files.forEach(fileInfo => {
    const repo = fileInfo.repo || 'unknown';
    if (!filesByRepo[repo]) filesByRepo[repo] = [];
    filesByRepo[repo].push(fileInfo.file);
  });

  console.log(`  ✅ Found in ${results.files.length} file(s):`);
  for (const [repo, files] of Object.entries(filesByRepo)) {
    console.log(`\n     📦 ${repo}/`);
    files.forEach(file => {
      console.log(`        • ${file}`);
    });
  }
  console.log('');

  // Show first 5 matches
  console.log('  📄 Preview (first 5 matches):');
  console.log('  ' + '─'.repeat(60));

  const uniqueMatches = [];
  const seenContexts = new Set();

  for (const match of results.matches) {
    const key = `${match.repo}:${match.file}:${match.context}`;
    if (!seenContexts.has(key)) {
      uniqueMatches.push(match);
      seenContexts.add(key);
    }
    if (uniqueMatches.length >= 5) break;
  }

  for (const match of uniqueMatches) {
    console.log(`  📄 ${match.repo}/${match.file} (line ${match.line}):`);
    const lines = match.context.split('\n');
    lines.forEach((line, idx) => {
      const prefix = idx === 1 ? '  → ' : '    ';
      const trimmed = line.slice(0, 80);
      console.log(`${prefix}${trimmed}`);
    });
    console.log('');
  }
}

/**
 * Display topic content in console format
 * @param {Object} result - Topic result
 */
function displayTopic(result) {
  if (result.error) {
    console.error(`\n  ❌ ${result.error}`);
    if (result.hint) {
      console.error(`\n  💡 ${result.hint}`);
    }
    if (result.availableTopics) {
      console.error(`\n  Available topics: ${result.availableTopics.join(', ')}`);
    }
    return;
  }

  console.log(`\n  📖 ${result.file}`);
  console.log('  ' + '─'.repeat(60));
  console.log('');

  // Display first 100 lines
  const lines = result.content.split('\n').slice(0, 100);
  lines.forEach(line => {
    const trimmed = line.slice(0, 100);
    console.log(`  ${trimmed}`);
  });

  if (result.truncated) {
    console.log('');
    console.log(`  ... (${result.totalLength - result.content.length} more characters)`);
  }
}

/**
 * Display topic list in console format
 * @param {Object} result - Topics result
 */
function displayTopics(result) {
  if (result.error) {
    console.error(`\n  ❌ ${result.error}`);
    if (result.hint) {
      console.error(`\n  💡 ${result.hint}`);
    }
    return;
  }

  console.log(`\n  📚 Available ABAP Reference Topics`);
  console.log(`  📁 Reference folder: ${result.referenceFolder}`);
  console.log('');
  console.log('  Topic                File');
  console.log('  ' + '─'.repeat(60));

  result.topics.forEach(({ topic, file }) => {
    const paddedTopic = topic.padEnd(20);
    console.log(`  ${paddedTopic} ${file}`);
  });
}

/**
 * List all reference repositories
 * @returns {Promise<Object>} List of repositories
 */
async function listRepositories() {
  const refFolder = detectReferenceFolder();
  const repos = await getReferenceRepositories();

  if (!refFolder) {
    return {
      error: 'Reference folder not found',
      hint: 'Configure referenceFolder in .abapGitAgent or clone to ~/abap-reference'
    };
  }

  return {
    referenceFolder: refFolder,
    repositories: repos
  };
}

/**
 * Display repositories in console format
 * @param {Object} result - Repositories result
 */
function displayRepositories(result) {
  if (result.error) {
    console.error(`\n  ❌ ${result.error}`);
    if (result.hint) {
      console.error(`\n  💡 ${result.hint}`);
    }
    return;
  }

  console.log(`\n  📚 ABAP Reference Repositories`);
  console.log(`  📁 Reference folder: ${result.referenceFolder}`);
  console.log('');

  if (result.repositories.length === 0) {
    console.log('  ⚠️  No repositories found.');
    console.log('');
    console.log('  Add repositories by cloning them to the reference folder:');
    console.log('    cd ' + result.referenceFolder);
    console.log('    git clone <repo-url>');
    return;
  }

  console.log(`  Found ${result.repositories.length} repository(ies):`);
  console.log('');
  console.log('  Repository                    Type');
  console.log('  ' + '─'.repeat(50));

  result.repositories.forEach(repo => {
    let type;
    if (repo.isCustomGuidelines) {
      type = 'Custom Guidelines';
    } else if (repo.isGitRepo) {
      type = 'Git Repo';
    } else {
      type = 'ABAP Folder';
    }
    const paddedName = repo.name.padEnd(30);
    console.log(`  ${paddedName} ${type}`);
  });
}

/**
 * Detect guidelines folder in current project
 * Looks for abap/guidelines/ folder
 * @returns {string|null} Path to guidelines folder or null if not found
 */
function detectGuidelinesFolder() {
  const cwd = process.cwd();
  const possiblePaths = [
    path.join(cwd, 'abap', 'guidelines'),
    path.join(cwd, 'guidelines'),
    path.join(cwd, 'docs', 'guidelines')
  ];

  for (const guidelinesPath of possiblePaths) {
    if (fs.existsSync(guidelinesPath)) {
      const stats = fs.statSync(guidelinesPath);
      if (stats.isDirectory()) {
        return guidelinesPath;
      }
    }
  }

  return null;
}

/**
 * Get all guideline files from the project
 * @returns {Promise<Array<{name: string, path: string, content: string}>>}
 */
async function getGuidelineFiles() {
  const guidelinesFolder = detectGuidelinesFolder();
  if (!guidelinesFolder) {
    return [];
  }

  const files = [];

  try {
    const entries = await readdir(guidelinesFolder);

    for (const entry of entries) {
      if (entry.endsWith('.md')) {
        const fullPath = path.join(guidelinesFolder, entry);
        const content = await readFile(fullPath, 'utf8');
        files.push({
          name: entry,
          path: fullPath,
          content,
          relativePath: path.join('guidelines', entry)
        });
      }
    }
  } catch (error) {
    // Return empty array on error
  }

  return files.sort((a, b) => a.name.localeCompare(b.name));
}

/**
 * Export guidelines to reference folder
 * Copies guideline files to the reference folder for searching
 * @returns {Promise<Object>} Export result
 */
async function exportGuidelines() {
  const guidelinesFolder = detectGuidelinesFolder();
  const refFolder = detectReferenceFolder();

  if (!guidelinesFolder) {
    return {
      success: false,
      error: 'No guidelines folder found',
      hint: 'Create abap/guidelines/ folder in your project'
    };
  }

  if (!refFolder) {
    return {
      success: false,
      error: 'Reference folder not found',
      hint: 'Configure referenceFolder in .abapGitAgent or clone to ~/abap-reference'
    };
  }

  const guidelineFiles = await getGuidelineFiles();

  if (guidelineFiles.length === 0) {
    return {
      success: false,
      error: 'No guideline files found',
      hint: 'Create .md files in abap/guidelines/'
    };
  }

  // Create a dedicated folder in reference for guidelines
  const exportPath = path.join(refFolder, 'custom-guidelines');

  try {
    // Create export directory if it doesn't exist
    if (!fs.existsSync(exportPath)) {
      fs.mkdirSync(exportPath, { recursive: true });
    }

    let exported = 0;

    for (const file of guidelineFiles) {
      const destPath = path.join(exportPath, file.name);
      fs.writeFileSync(destPath, file.content);
      exported++;
    }

    return {
      success: true,
      message: `Exported ${exported} guideline(s) to reference folder`,
      sourceFolder: guidelinesFolder,
      exportFolder: exportPath,
      files: guidelineFiles.map(f => f.name)
    };
  } catch (error) {
    return {
      success: false,
      error: `Failed to export: ${error.message}`
    };
  }
}

/**
 * Search guidelines in current project
 * @param {string} pattern - Pattern to search for
 * @returns {Promise<Object>} Search results
 */
async function searchGuidelines(pattern) {
  const guidelineFiles = await getGuidelineFiles();

  if (guidelineFiles.length === 0) {
    return {
      pattern,
      guidelinesFound: false,
      message: 'No guideline files found in project'
    };
  }

  const results = {
    pattern,
    guidelinesFound: true,
    files: [],
    matches: []
  };

  for (const file of guidelineFiles) {
    if (file.content.toLowerCase().includes(pattern.toLowerCase())) {
      results.files.push(file.relativePath);

      // Find matching lines with context
      const lines = file.content.split('\n');

      for (let i = 0; i < lines.length; i++) {
        if (lines[i].toLowerCase().includes(pattern.toLowerCase())) {
          const start = Math.max(0, i - 1);
          const end = Math.min(lines.length, i + 2);
          const context = lines.slice(start, end).join('\n');

          results.matches.push({
            file: file.relativePath,
            line: i + 1,
            context
          });
        }
      }
    }
  }

  return results;
}

/**
 * Display export result
 * @param {Object} result - Export result
 */
function displayExportResult(result) {
  if (result.success) {
    console.log(`\n  ✅ ${result.message}`);
    console.log(`\n  📁 Source: ${result.sourceFolder}`);
    console.log(`  📁 Export: ${result.exportFolder}`);
    console.log(`\n  Files exported:`);
    for (const file of result.files) {
      console.log(`    - ${file}`);
    }
    console.log(`\n  💡 These guidelines will now be searchable via 'ref' command`);
  } else {
    console.error(`\n  ❌ ${result.error}`);
    if (result.hint) {
      console.error(`\n  💡 ${result.hint}`);
    }
  }
}

module.exports = {
  detectReferenceFolder,
  detectGuidelinesFolder,
  getReferenceRepositories,
  getGuidelineFiles,
  searchPattern,
  searchGuidelines,
  getTopic,
  listTopics,
  listRepositories,
  exportGuidelines,
  displaySearchResults,
  displayTopic,
  displayTopics,
  displayRepositories,
  displayExportResult,
  TOPIC_MAP
};
