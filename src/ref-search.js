/**
 * ABAP Reference Search - Search ABAP cheat sheets for patterns
 *
 * This module provides portable reference lookup that works regardless
 * of where the user stores their ABAP cheat sheets.
 */

const fs = require('fs');
const path = require('path');
const { promisify } = require('util');
const readdir = promisify(fs.readdir);
const readFile = promisify(fs.readFile);

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
 * Search for a pattern in cheat sheets
 * @param {string} pattern - Pattern to search for
 * @returns {Promise<Object>} Search results
 */
async function searchPattern(pattern) {
  const cheatSheetsDir = getCheatSheetsDir();

  if (!cheatSheetsDir) {
    return {
      error: 'Reference folder not found',
      hint: 'Configure referenceFolder in .abapGitAgent or clone to ~/abap-reference'
    };
  }

  const results = {
    pattern,
    referenceFolder: path.dirname(cheatSheetsDir),
    files: [],
    matches: []
  };

  try {
    const files = await readdir(cheatSheetsDir);
    const mdFiles = files.filter(f => f.endsWith('.md'));

    for (const file of mdFiles) {
      const filePath = path.join(cheatSheetsDir, file);
      const content = await readFile(filePath, 'utf8');

      if (content.toLowerCase().includes(pattern.toLowerCase())) {
        results.files.push(file);

        // Find matching lines with context
        const lines = content.split('\n');
        for (let i = 0; i < lines.length; i++) {
          if (lines[i].toLowerCase().includes(pattern.toLowerCase())) {
            const start = Math.max(0, i - 1);
            const end = Math.min(lines.length, i + 2);
            const context = lines.slice(start, end).join('\n');

            results.matches.push({
              file,
              line: i + 1,
              context
            });

            // Limit matches per file to avoid overwhelming output
            if (results.matches.filter(m => m.file === file).length >= 3) {
              break;
            }
          }
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
  console.log('');

  if (results.files.length === 0) {
    console.log('  ⚠️  No matches found.');
    return;
  }

  console.log(`  ✅ Found in ${results.files.length} file(s):`);
  results.files.forEach(file => {
    console.log(`     • ${file}`);
  });
  console.log('');

  // Show first 5 matches
  console.log('  📄 Preview (first 5 matches):');
  console.log('  ' + '─'.repeat(60));

  const uniqueMatches = [];
  const seenContexts = new Set();

  for (const match of results.matches) {
    const key = `${match.file}:${match.context}`;
    if (!seenContexts.has(key)) {
      uniqueMatches.push(match);
      seenContexts.add(key);
    }
    if (uniqueMatches.length >= 5) break;
  }

  for (const match of uniqueMatches) {
    console.log(`  📄 ${match.file} (line ${match.line}):`);
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

module.exports = {
  detectReferenceFolder,
  searchPattern,
  getTopic,
  listTopics,
  displaySearchResults,
  displayTopic,
  displayTopics,
  TOPIC_MAP
};
