/**
 * ABAP Reference Search - Search ABAP reference repositories for patterns
 *
 * This module provides portable reference lookup across multiple ABAP repositories
 * including cheat sheets and any other ABAP code repositories in the reference folder.
 */

const fs = require('fs');
const path = require('path');
const { promisify } = require('util');
const { execSync } = require('child_process');
const readdir = promisify(fs.readdir);
const readFile = promisify(fs.readFile);
const stat = promisify(fs.stat);

// Topic to file mapping
const TOPIC_MAP = {
  'internal-tables': '01_Internal_Tables.md',
  'structures': '02_Structures.md',
  'sql-cheatsheet': '03_ABAP_SQL.md',
  'oop': '04_ABAP_Object_Orientation.md',
  'oop-cheatsheet': '04_ABAP_Object_Orientation.md',
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
  'cds-cheatsheet': '15_CDS_View_Entities.md',
  'datatypes': '16_Data_Types_and_Objects.md',
  'luw': '17_SAP_LUW.md',
  'dynpro': '18_Dynpro.md',
  'cloud': '19_ABAP_for_Cloud_Development.md',
  'selection-screens': '20_Selection_Screens_Lists.md',
  'json-xml': '21_XML_JSON.md',
  'json-cheatsheet': '21_XML_JSON.md',
  'xml-cheatsheet': '21_XML_JSON.md',
  'released-classes': '22_Released_ABAP_Classes.md',
  'datetime': '23_Date_and_Time.md',
  'functions': '24_Builtin_Functions.md',
  'auth': '25_Authorization_Checks.md',
  'authorization': '25_Authorization_Checks.md',
  'dictionary': '26_ABAP_Dictionary.md',
  'exceptions-cheatsheet': '27_Exceptions.md',
  'exception-cheatsheet': '27_Exceptions.md',
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
 * Ensure reference folder exists, create if necessary
 * @returns {string|null} Path to reference folder or null if cannot create
 */
function ensureReferenceFolder() {
  let refFolder = detectReferenceFolder();

  if (refFolder && fs.existsSync(refFolder)) {
    return refFolder;
  }

  // Try to create the reference folder at default location
  const homeDir = require('os').homedir();
  const defaultPath = path.join(homeDir, 'abap-reference');

  try {
    if (!fs.existsSync(defaultPath)) {
      fs.mkdirSync(defaultPath, { recursive: true });
    }
    return defaultPath;
  } catch (error) {
    return null;
  }
}

/**
 * Clone a repository to the reference folder
 * @param {string} repoUrl - Git repository URL or short name (e.g., "SAP-samples/abap-cheat-sheets")
 * @param {string|null} name - Optional folder name for the cloned repo
 * @returns {Object} Clone result
 */
function cloneRepository(repoUrl, name = null) {
  const refFolder = ensureReferenceFolder();

  if (!refFolder) {
    return {
      success: false,
      error: 'Could not create reference folder',
      hint: 'Set referenceFolder in .abapGitAgent or ensure ~/abap-reference is writable'
    };
  }

  // Parse the repo URL
  let targetName = name;
  let cloneUrl = repoUrl;

  // Handle short names (e.g., "SAP-samples/abap-cheat-sheets")
  if (!repoUrl.startsWith('http://') && !repoUrl.startsWith('https://') && !repoUrl.startsWith('git@')) {
    cloneUrl = `https://github.com/${repoUrl}.git`;
  }

  // If no name provided, extract from URL
  if (!targetName) {
    // Extract repo name from URL
    const urlParts = cloneUrl.split('/');
    const repoWithGit = urlParts[urlParts.length - 1];
    targetName = repoWithGit.replace(/\.git$/, '');
  }

  const targetPath = path.join(refFolder, targetName);

  // Check if already exists
  if (fs.existsSync(targetPath)) {
    return {
      success: false,
      error: `Repository already exists: ${targetName}`,
      hint: `Delete '${targetPath}' to re-clone, or use --name to specify a different folder name`,
      existingPath: targetPath
    };
  }

  try {
    // Run git clone
    execSync(`git clone "${cloneUrl}" "${targetPath}"`, {
      stdio: 'pipe',
      encoding: 'utf8'
    });

    return {
      success: true,
      message: `Successfully cloned ${repoUrl}`,
      repository: targetName,
      folder: targetPath,
      referenceFolder: refFolder
    };
  } catch (error) {
    return {
      success: false,
      error: `Failed to clone: ${error.message}`,
      hint: 'Check the repository URL and your network connection'
    };
  }
}

/**
 * Display clone result in console format
 * @param {Object} result - Clone result
 */
function displayCloneResult(result) {
  if (result.success) {
    console.log(`\n  ✅ ${result.message}`);
    console.log(`\n  📁 Repository: ${result.repository}`);
    console.log(`  📁 Location: ${result.folder}`);
    console.log(`  📁 Reference folder: ${result.referenceFolder}`);
    console.log(`\n  💡 You can now search this repository with:`);
    console.log(`     abapgit-agent ref --list-repos`);
  } else {
    console.error(`\n  ❌ ${result.error}`);
    if (result.hint) {
      console.error(`\n  💡 ${result.hint}`);
    }
  }
}

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
 * Search for a pattern across all reference repositories and local guidelines
 * @param {string} pattern - Pattern to search for
 * @returns {Promise<Object>} Search results
 */
async function searchPattern(pattern) {
  const refFolder = detectReferenceFolder();
  const repos = await getReferenceRepositories();
  const guidelinesFolder = detectGuidelinesFolder();
  const builtInPath = getBuiltInGuidelinesPath();

  // If no sources at all, return error
  if (!refFolder && !guidelinesFolder && !builtInPath) {
    return {
      error: 'Reference folder not found',
      hint: 'Configure referenceFolder in .abapGitAgent, clone to ~/abap-reference, or create abap/guidelines/ folder'
    };
  }

  const results = {
    pattern,
    referenceFolder: refFolder,
    guidelinesFolder: guidelinesFolder,
    repositories: repos.map(r => r.name),
    files: [],
    matches: []
  };

  /**
   * Helper: search a list of guideline file objects and push results
   */
  function searchGuidelineFiles(guidelineFiles, repoLabel) {
    for (const file of guidelineFiles) {
      if (file.content.toLowerCase().includes(pattern.toLowerCase())) {
        results.files.push({ repo: repoLabel, file: file.relativePath });

        const lines = file.content.split('\n');
        let matchCount = 0;
        for (let i = 0; i < lines.length; i++) {
          if (lines[i].toLowerCase().includes(pattern.toLowerCase())) {
            const start = Math.max(0, i - 1);
            const end = Math.min(lines.length, i + 2);
            results.matches.push({
              repo: repoLabel,
              file: file.relativePath,
              line: i + 1,
              context: lines.slice(start, end).join('\n')
            });
            matchCount++;
            if (matchCount >= 3) break;
          }
        }
      }
    }
  }

  try {
    // Search reference repositories if available
    if (repos.length > 0) {
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
    }

    // Tier 1: search local guidelines folder
    if (guidelinesFolder) {
      const localFiles = await getGuidelineFilesFromPath(guidelinesFolder, 'guidelines');
      searchGuidelineFiles(localFiles, 'guidelines');
    }

    // Tier 2: search built-in guidelines for files not shadowed by local ones
    if (builtInPath) {
      const localFileNames = guidelinesFolder
        ? new Set(fs.readdirSync(guidelinesFolder).filter(f => f.endsWith('.md')))
        : new Set();
      const builtInFiles = await getGuidelineFilesFromPath(builtInPath, 'guidelines');
      const filesToSearch = builtInFiles.filter(f => !localFileNames.has(f.name));
      // Override relativePath to signal built-in source
      filesToSearch.forEach(f => { f.relativePath = path.join('guidelines', f.name); });
      searchGuidelineFiles(filesToSearch, '[built-in]');
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
  const topicLower = topic.toLowerCase();

  // First, check local guidelines folder
  const guidelinesDir = detectGuidelinesFolder();
  if (guidelinesDir) {
    // Map topic to local guideline file
    const guidelineMap = {
      'abapgit': 'abapgit.md',
      'abapgit-xml-only': 'abapgit-xml-only.md',
      'abapgit-fugr': 'abapgit-fugr.md',
      'xml': 'objects.md',
      'objects': 'objects.md',
      'naming': 'objects.md'
    };

    const guidelineFile = guidelineMap[topicLower];
    if (guidelineFile) {
      const guidelinePath = path.join(guidelinesDir, guidelineFile);
      if (fs.existsSync(guidelinePath)) {
        try {
          const content = await readFile(guidelinePath, 'utf8');
          return {
            topic,
            file: guidelineFile,
            content: content.slice(0, 15000),
            truncated: content.length > 15000,
            totalLength: content.length,
            source: 'guidelines'
          };
        } catch (error) {
          // Continue to check external reference
        }
      }
    }
  }

  // Fall back to cheat sheets TOPIC_MAP
  const cheatSheetsDir = getCheatSheetsDir();

  const fileName = TOPIC_MAP[topicLower];
  if (fileName && cheatSheetsDir) {
    const filePath = path.join(cheatSheetsDir, fileName);
    if (fs.existsSync(filePath)) {
      try {
        const content = await readFile(filePath, 'utf8');
        return {
          topic,
          file: fileName,
          content: content.slice(0, 5000),
          truncated: content.length > 5000,
          totalLength: content.length,
          source: 'cheat-sheets'
        };
      } catch (error) {
        // fall through
      }
    }
  }

  // TOPIC_MAP matched but cheat sheets not available — give a helpful setup hint
  if (fileName && !cheatSheetsDir) {
    // Distinguish between "not configured" and "configured but folder missing"
    let configuredRefFolder = null;
    try {
      const configPath = path.join(process.cwd(), '.abapGitAgent');
      if (fs.existsSync(configPath)) {
        const config = JSON.parse(fs.readFileSync(configPath, 'utf8'));
        if (config.referenceFolder) configuredRefFolder = config.referenceFolder;
      }
    } catch (e) { /* ignore */ }

    if (configuredRefFolder) {
      return {
        error: `Topic '${topic}' requires SAP ABAP cheat sheets (folder not found: ${configuredRefFolder})`,
        hint: [
          'The referenceFolder is configured but does not exist on disk.',
          `Clone the cheat sheets into it: abapgit-agent ref --clone SAP-samples/abap-cheat-sheets`,
          '',
          'Bundled topics (no setup needed): run abapgit-agent ref --list-topics'
        ].join('\n')
      };
    }

    return {
      error: `Topic '${topic}' requires SAP ABAP cheat sheets (not configured)`,
      hint: [
        'To access SAP cheat sheet topics:',
        '  1. Add to .abapGitAgent: { "referenceFolder": "/path/to/abap-reference" }',
        '  2. Clone: abapgit-agent ref --clone SAP-samples/abap-cheat-sheets',
        '',
        'Bundled topics (no setup needed): run abapgit-agent ref --list-topics'
      ].join('\n')
    };
  }

  // Fall back to bundled guidelines by filename stem
  // e.g. 'debug-session' → 'debug-session.md', 'debug' → 'debug-session.md' (if unambiguous)
  const builtInPath = getBuiltInGuidelinesPath();
  if (builtInPath) {
    try {
      const builtInFiles = fs.readdirSync(builtInPath).filter(f => f.endsWith('.md'));

      // Exact stem match first
      const exactMatch = builtInFiles.find(f => f.replace(/\.md$/, '') === topicLower);
      if (exactMatch) {
        const content = await readFile(path.join(builtInPath, exactMatch), 'utf8');
        return {
          topic,
          file: exactMatch,
          content: content.slice(0, 15000),
          truncated: content.length > 15000,
          totalLength: content.length,
          source: 'built-in guidelines'
        };
      }

      // Partial stem match (unambiguous only)
      const partialMatches = builtInFiles.filter(f => f.replace(/\.md$/, '').includes(topicLower));
      if (partialMatches.length === 1) {
        const content = await readFile(path.join(builtInPath, partialMatches[0]), 'utf8');
        return {
          topic,
          file: partialMatches[0],
          content: content.slice(0, 15000),
          truncated: content.length > 15000,
          totalLength: content.length,
          source: 'built-in guidelines'
        };
      } else if (partialMatches.length > 1) {
        return {
          error: `Ambiguous topic: '${topic}' matches multiple guideline files`,
          hint: `Be more specific. Matches: ${partialMatches.map(f => f.replace(/\.md$/, '')).join(', ')}`
        };
      }
    } catch (error) {
      // fall through to final error
    }
  }

  return {
    error: `Unknown topic: ${topic}`,
    hint: 'For project guidelines, use the filename stem (e.g. --topic debug-session, --topic workflow-detailed)\nRun: abapgit-agent ref --list-topics'
  };
}

/**
 * List available topics
 * @returns {Promise<Object>} List of topics
 */
async function listTopics() {
  const cheatSheetsDir = getCheatSheetsDir();

  // Build topic list from cheat sheet files that exist
  const topics = [];

  if (cheatSheetsDir) {
    const seenFiles = new Set();
    for (const [topic, file] of Object.entries(TOPIC_MAP)) {
      if (!seenFiles.has(file) && fs.existsSync(path.join(cheatSheetsDir, file))) {
        topics.push({ topic, file });
        seenFiles.add(file);
      }
    }
  }

  // Build guideline topic list from bundled guidelines (filename stem → filename)
  const builtInPath = getBuiltInGuidelinesPath();
  const guidelineTopics = [];
  if (builtInPath) {
    try {
      const entries = fs.readdirSync(builtInPath).filter(f => f.endsWith('.md')).sort();
      for (const file of entries) {
        const stem = file.replace(/\.md$/, '');
        guidelineTopics.push({ topic: stem, file });
      }
    } catch (e) { /* ignore */ }
  }

  // Also include local guidelines/ topics (if present)
  const localGuidelinesDir = detectGuidelinesFolder();
  const localGuidelineTopics = [];
  if (localGuidelinesDir) {
    try {
      const entries = fs.readdirSync(localGuidelinesDir).filter(f => f.endsWith('.md')).sort();
      for (const file of entries) {
        const stem = file.replace(/\.md$/, '');
        localGuidelineTopics.push({ topic: stem, file });
      }
    } catch (e) { /* ignore */ }
  }

  return {
    referenceFolder: cheatSheetsDir ? path.dirname(cheatSheetsDir) : null,
    topics: topics.sort((a, b) => a.file.localeCompare(b.file)),
    guidelineTopics,
    localGuidelineTopics
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

  // Show which sources were searched
  const sources = [];
  if (results.referenceFolder) {
    sources.push('reference repositories');
  }
  if (results.guidelinesFolder) {
    sources.push('local guidelines');
  }
  // Check if built-in guidelines were searched (present in matches)
  const hasBuiltIn = results.files.some(f => f.repo === '[built-in]');
  if (hasBuiltIn) {
    sources.push('built-in guidelines');
  }
  console.log(`  📁 Sources searched: ${sources.join(', ') || 'none'}`);

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
    const icon = repo === 'guidelines' ? '📋' : repo === '[built-in]' ? '📦' : '📦';
    const label = repo === '[built-in]' ? 'built-in guidelines' : repo;
    console.log(`\n     ${icon} ${label}/`);
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

  const sourceLabel = result.source === 'built-in guidelines' ? ' [built-in]' : result.source === 'guidelines' ? ' [local]' : '';
  console.log(`\n  📖 ${result.file}${sourceLabel}`);
  console.log('  ' + '─'.repeat(60));
  console.log('');

  // Display up to 300 lines (covers full guideline files without truncation)
  const lines = result.content.split('\n').slice(0, 300);
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
  if (result.referenceFolder) {
    console.log(`  📁 Reference folder: ${result.referenceFolder}`);
  }
  console.log('');

  if (result.topics.length > 0) {
    console.log('  SAP Cheat Sheets');
    console.log('  Topic                File');
    console.log('  ' + '─'.repeat(60));
    result.topics.forEach(({ topic, file }) => {
      console.log(`  ${topic.padEnd(20)} ${file}`);
    });
    console.log('');
  }

  if (result.localGuidelineTopics && result.localGuidelineTopics.length > 0) {
    console.log('  Local Guidelines  (guidelines/)');
    console.log('  Topic                File');
    console.log('  ' + '─'.repeat(60));
    result.localGuidelineTopics.forEach(({ topic, file }) => {
      console.log(`  ${topic.padEnd(20)} ${file}`);
    });
    console.log('');
  }

  if (result.guidelineTopics && result.guidelineTopics.length > 0) {
    console.log('  Bundled Guidelines  (use: abapgit-agent ref --topic <topic>)');
    console.log('  Topic                File');
    console.log('  ' + '─'.repeat(60));
    result.guidelineTopics.forEach(({ topic, file }) => {
      console.log(`  ${topic.padEnd(20)} ${file}`);
    });
    console.log('');
  }
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
 * Get the path to the built-in guidelines in abapgit-agent
 * @returns {string|null} Path to built-in guidelines or null if not found
 */
function getBuiltInGuidelinesPath() {
  // Try to find the guidelines relative to this module
  // This allows the CLI to work from any location
  const possiblePaths = [
    // When running from source (bin/abapgit-agent)
    path.join(__dirname, '..', 'abap', 'guidelines'),
    // When running from installed package
    path.join(__dirname, '..', '..', 'abap', 'guidelines')
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
 * Initialize guidelines in current project by copying from abapgit-agent
 * @returns {Object} Init result
 */
function initGuidelines() {
  const builtInPath = getBuiltInGuidelinesPath();

  if (!builtInPath) {
    return {
      success: false,
      error: 'Built-in guidelines not found',
      hint: 'Make sure abapgit-agent is properly installed'
    };
  }

  const cwd = process.cwd();
  const targetPath = path.join(cwd, 'abap', 'guidelines');

  // Check if guidelines already exist
  if (fs.existsSync(targetPath)) {
    return {
      success: false,
      error: 'Guidelines folder already exists',
      hint: `Delete or rename '${targetPath}' first, then run --init again`,
      existingPath: targetPath
    };
  }

  try {
    // Create target directory
    fs.mkdirSync(targetPath, { recursive: true });

    // Copy all files from built-in guidelines
    const files = fs.readdirSync(builtInPath);
    let copied = 0;

    for (const file of files) {
      if (file.endsWith('.md')) {
        const srcPath = path.join(builtInPath, file);
        const destPath = path.join(targetPath, file);
        const content = fs.readFileSync(srcPath, 'utf8');
        fs.writeFileSync(destPath, content);
        copied++;
      }
    }

    if (copied === 0) {
      return {
        success: false,
        error: 'No guideline files found in built-in guidelines'
      };
    }

    return {
      success: true,
      message: `Initialized ${copied} guideline(s) in your project`,
      sourceFolder: builtInPath,
      targetFolder: targetPath,
      files: files.filter(f => f.endsWith('.md'))
    };
  } catch (error) {
    return {
      success: false,
      error: `Failed to initialize: ${error.message}`
    };
  }
}

/**
 * Get all guideline files from a specific path
 * @param {string} guidelinesPath - Path to guidelines folder
 * @param {string} [label] - Optional label suffix for relativePath (default: 'guidelines')
 * @returns {Promise<Array<{name: string, path: string, content: string, relativePath: string}>>}
 */
async function getGuidelineFilesFromPath(guidelinesPath, label) {
  const folderLabel = label || 'guidelines';
  const files = [];
  try {
    const entries = await readdir(guidelinesPath);
    for (const entry of entries) {
      if (entry.endsWith('.md')) {
        const fullPath = path.join(guidelinesPath, entry);
        const content = await readFile(fullPath, 'utf8');
        files.push({
          name: entry,
          path: fullPath,
          content,
          relativePath: path.join(folderLabel, entry)
        });
      }
    }
  } catch (error) {
    // Return empty array on error
  }
  return files.sort((a, b) => a.name.localeCompare(b.name));
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
  return getGuidelineFilesFromPath(guidelinesFolder);
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

/**
 * Display init result
 * @param {Object} result - Init result
 */
function displayInitResult(result) {
  if (result.success) {
    console.log(`\n  ✅ ${result.message}`);
    console.log(`\n  📁 Source: ${result.sourceFolder}`);
    console.log(`  📁 Created: ${result.targetFolder}`);
    console.log(`\n  Files copied:`);
    for (const file of result.files) {
      console.log(`    - ${file}`);
    }
    console.log(`\n  💡 Next steps:`);
    console.log(`    1. Review the guidelines in abap/guidelines/`);
    console.log(`    2. Customize as needed for your project`);
    console.log(`    3. Guidelines are automatically searchable with 'ref' command`);
  } else {
    console.error(`\n  ❌ ${result.error}`);
    if (result.hint) {
      console.error(`\n  💡 ${result.hint}`);
    }
  }
}

module.exports = {
  detectReferenceFolder,
  ensureReferenceFolder,
  detectGuidelinesFolder,
  getBuiltInGuidelinesPath,
  getGuidelineFilesFromPath,
  initGuidelines,
  getReferenceRepositories,
  getGuidelineFiles,
  searchPattern,
  searchGuidelines,
  getTopic,
  listTopics,
  listRepositories,
  exportGuidelines,
  cloneRepository,
  displaySearchResults,
  displayTopic,
  displayTopics,
  displayRepositories,
  displayExportResult,
  displayInitResult,
  displayCloneResult,
  TOPIC_MAP
};
