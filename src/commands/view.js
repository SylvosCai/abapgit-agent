/**
 * View command - View ABAP object definitions
 */

const fs = require('fs');
const path = require('path');

/**
 * Find the local .clas.abap file for an object by scanning the configured
 * source folder (from the nearest .abapGitAgent config file).
 * Returns the file path if found, null otherwise.
 */
function findLocalClassFile(objName) {
  try {
    // Try to read .abapGitAgent to get configured folder
    let folder = null;
    let dir = process.cwd();
    for (let i = 0; i < 5; i++) {
      const cfgPath = path.join(dir, '.abapGitAgent');
      if (fs.existsSync(cfgPath)) {
        try {
          const cfg = JSON.parse(fs.readFileSync(cfgPath, 'utf8'));
          folder = cfg.folder;
        } catch (e) { /* ignore */ }
        break;
      }
      const parent = path.dirname(dir);
      if (parent === dir) break;
      dir = parent;
    }

    // Normalise folder to a relative path segment (strip leading/trailing slashes)
    const folderSeg = folder ? folder.replace(/^\/|\/$/g, '') : null;
    const lowerName = objName.toLowerCase();
    const fileName = `${lowerName}.clas.abap`;

    const candidates = [];
    if (folderSeg) {
      candidates.push(path.join(process.cwd(), folderSeg, fileName));
    }
    candidates.push(path.join(process.cwd(), 'src', fileName));
    candidates.push(path.join(process.cwd(), 'abap', fileName));
    candidates.push(path.join(process.cwd(), fileName));

    for (const candidate of candidates) {
      if (fs.existsSync(candidate)) return candidate;
    }
  } catch (e) { /* ignore */ }
  return null;
}

/**
 * Given a fully assembled class source (array of lines, 1-indexed positions),
 * return a map of { METHODNAME_UPPER: globalLineNumber } where globalLineNumber
 * is the line on which `METHOD <name>.` appears.
 *
 * Matches lines where the first non-blank token is exactly "METHOD" (case-insensitive)
 * to avoid false matches on comments or string literals.
 */
function buildMethodLineMap(sourceLines) {
  const map = {};
  for (let i = 0; i < sourceLines.length; i++) {
    const condensed = sourceLines[i].trimStart();
    if (/^method\s+/i.test(condensed)) {
      // Extract method name: everything between "METHOD " and the next space/period/paren
      const m = condensed.match(/^method\s+([\w~]+)/i);
      if (m) {
        map[m[1].toUpperCase()] = i + 1; // 1-based line number
      }
    }
  }
  return map;
}

/**
 * Fetch the assembled class source from ADT.
 * Returns an array of source lines, or null on failure.
 */
async function fetchAdtSource(objName, config) {
  try {
    const { AdtHttp } = require('../utils/adt-http');
    const adt = new AdtHttp(config);
    await adt.fetchCsrfToken();
    const lower = objName.toLowerCase();
    const resp = await adt.get(
      `/sap/bc/adt/oo/classes/${lower}/source/main`,
      { accept: 'text/plain' }
    );
    if (resp && resp.body) {
      return resp.body.split('\n');
    }
  } catch (e) { /* ignore — fall through */ }
  return null;
}

/**
 * Compute global_start for each CM section in a sections array.
 * Mutates sections in-place, adding a globalStart property.
 *
 * Strategy:
 *  1. Try local .clas.abap file → build method line map
 *  2. Fall back to ADT source fetch → build method line map
 *  3. If neither works, leave globalStart = 0 (unknown)
 *
 * For non-CM sections (CU, CO, CP, CCDEF, CCIMP, CCAU) globalStart is also
 * set from the source map for sections that have a unique recognisable first line,
 * but for simplicity we set it to 0 for non-CM sections (they use section-local
 * line numbers already).
 */
async function computeGlobalStarts(objName, sections, config) {
  // Only CM sections need global_start for breakpoints
  const cmSections = sections.filter(s => {
    const suffix = s.SUFFIX || s.suffix || '';
    const methodName = s.METHOD_NAME || s.method_name || '';
    return suffix.startsWith('CM') && methodName;
  });
  if (cmSections.length === 0) return;

  let sourceLines = null;

  // Try local file first
  const localFile = findLocalClassFile(objName);
  if (localFile) {
    try {
      sourceLines = fs.readFileSync(localFile, 'utf8').split('\n');
    } catch (e) { /* ignore */ }
  }

  // Fall back to ADT source fetch
  if (!sourceLines) {
    sourceLines = await fetchAdtSource(objName, config);
  }

  if (!sourceLines) return;

  const methodLineMap = buildMethodLineMap(sourceLines);

  for (const section of cmSections) {
    const methodName = (section.METHOD_NAME || section.method_name || '').toUpperCase();
    if (methodLineMap[methodName] !== undefined) {
      section.globalStart = methodLineMap[methodName];
    }
  }
}

/**
 * Given the lines of a CM method section, return the 0-based index of the
 * first "executable" line — i.e. skip METHOD, blank lines, comment lines,
 * and declaration lines (DATA/FINAL/TYPES/CONSTANTS/CLASS-DATA), including
 * multi-line DATA: blocks whose continuation lines end with a period.
 * Returns 0 if no better line is found (falls back to METHOD statement).
 */
function findFirstExecutableLine(lines) {
  const declPattern = /^\s*(data|final|types|constants|class-data)[\s:(]/i;
  const methodPattern = /^\s*method\s+/i;
  const commentPattern = /^\s*[*"]/;
  // Program-level header/declaration keywords that are not executable statements
  const progDeclPattern = /^\s*(report|program|parameters|tables|selection-screen|select-options|class-pool|function-pool|interface-pool|type-pool|include)\b/i;
  let inDeclBlock = false; // true while inside a multi-line DATA:/TYPES:/PARAMETERS: block
  for (let i = 0; i < lines.length; i++) {
    const trimmed = lines[i].trim();
    if (inDeclBlock) {
      // continuation line — skip until the block closes with a period
      if (trimmed.endsWith('.')) inDeclBlock = false;
      continue;
    }
    if (!trimmed) continue;             // blank line
    if (methodPattern.test(trimmed)) continue; // METHOD statement itself
    if (commentPattern.test(trimmed)) continue; // comment line
    if (declPattern.test(trimmed)) {
      // Multi-line block (DATA: ...,\n  ...) stays open until period
      if (!trimmed.endsWith('.')) inDeclBlock = true;
      continue;
    }
    if (progDeclPattern.test(trimmed)) {
      // Multi-line block (PARAMETERS: ...,\n  ...) stays open until period
      if (!trimmed.endsWith('.')) inDeclBlock = true;
      continue;
    }
    return i;
  }
  return 0;
}

module.exports = {
  name: 'view',
  description: 'View ABAP object definitions from ABAP system',
  requiresAbapConfig: true,
  requiresVersionCheck: true,
  _buildMethodLineMap: buildMethodLineMap,
  _findFirstExecutableLine: findFirstExecutableLine,

  async execute(args, context) {
    const { loadConfig, AbapHttp } = context;

    if (args.includes('--help') || args.includes('-h')) {
      console.log(`
Usage:
  abapgit-agent view --objects <obj1>,<obj2>,... [--type <type>] [--full] [--lines] [--json]
  abapgit-agent view --objects <FUGR> --type FUGR [--full] [--fm <name>] [--lines]

Description:
  View ABAP object definitions from the ABAP system.

Parameters:
  --objects <obj1,...>  Comma-separated object names (required).
  --type <type>         Object type: CLAS, INTF, PROG, TABL, STRU, DTEL, TTYP, DOMA,
                        DDLS, DCLS, MSAG, FUGR (auto-detected from TADIR if omitted).
  --full                Show full source including all method implementations.
  --lines               Show dual line numbers (G = global for debug set, [N] = include-local).
  --fm <name>           With --full: show only the specified function module (FUGR only).
  --json                Output as JSON.

Examples:
  abapgit-agent view --objects ZCL_MY_CLASS
  abapgit-agent view --objects ZCL_MY_CLASS --full
  abapgit-agent view --objects ZCL_MY_CLASS --full --lines
  abapgit-agent view --objects ZMY_TABLE --type TABL
  abapgit-agent view --objects SUSR --type FUGR --full --fm AUTHORITY_CHECK --lines
  abapgit-agent view --objects ZCL_FOO,ZIF_BAR
`);
      return;
    }

    const objectsArgIndex = args.indexOf('--objects');
    if (objectsArgIndex === -1 || objectsArgIndex + 1 >= args.length) {
      console.error('Error: --objects parameter required');
      console.error('Usage: abapgit-agent view --objects <obj1>,<obj2>,... [--type <type>] [--json]');
      console.error('Example: abapgit-agent view --objects ZCL_MY_CLASS');
      console.error('Example: abapgit-agent view --objects ZCL_CLASS1,ZCL_CLASS2 --type CLAS');
      process.exit(1);
    }

    const objects = args[objectsArgIndex + 1].split(',').map(o => o.trim().toUpperCase());
    const typeArg = args.indexOf('--type');
    const type = typeArg !== -1 ? args[typeArg + 1].toUpperCase() : null;
    const jsonOutput = args.includes('--json');
    const fullMode = args.includes('--full');
    const linesMode = args.includes('--lines');
    const fmArgIndex = args.indexOf('--fm');
    const fmName = fmArgIndex !== -1 && fmArgIndex + 1 < args.length
      ? args[fmArgIndex + 1].toUpperCase()
      : null;

    if (fmName && !fullMode) {
      console.error('  Error: --fm requires --full');
      process.exit(1);
    }

    console.log(`\n  Viewing ${objects.length} object(s)`);

    const config = loadConfig();
    const http = new AbapHttp(config);
    const csrfToken = await http.fetchCsrfToken();

    const data = {
      objects: objects
    };

    if (type) {
      data.type = type;
    }

    if (fullMode) {
      data.full = true;
    }

    if (fmName) {
      data.fm = fmName;
    }

    const result = await http.post('/sap/bc/z_abapgit_agent/view', data, { csrfToken });

    // Handle uppercase keys from ABAP
    const success = result.SUCCESS || result.success;
    const viewObjects = result.OBJECTS || result.objects || [];
    const message = result.MESSAGE || result.message || '';
    const error = result.ERROR || result.error;

    if (!success || error) {
      console.error(`\n  Error: ${error || 'Failed to view objects'}`);
      return;
    }

    // In full+lines mode, compute global line numbers client-side before rendering
    if (fullMode && linesMode) {
      for (const obj of viewObjects) {
        const objName = obj.NAME || obj.name || '';
        const sections = obj.SECTIONS || obj.sections || [];
        if (sections.length > 0 && objName) {
          await computeGlobalStarts(objName, sections, config);
        }
      }
    }

    if (jsonOutput) {
      console.log(JSON.stringify(result, null, 2));
    } else {
      console.log(`\n  ${message}`);
      console.log('');

      for (let i = 0; i < viewObjects.length; i++) {
        const obj = viewObjects[i];
        const objName = obj.NAME || obj.name || `Object ${i + 1}`;
        const objType = obj.TYPE || obj.type || '';
        const objTypeText = obj.TYPE_TEXT || obj.type_text || '';
        const description = obj.DESCRIPTION || obj.description || '';
        const methods = obj.METHODS || obj.methods || [];
        const components = obj.COMPONENTS || obj.components || [];
        const notFound = obj.NOT_FOUND || obj.not_found || false;

        // Check if object was not found
        if (notFound) {
          console.log(`  ❌ ${objName} (${objTypeText})`);
          if (description) {
            console.log(`     ${description}`);
          } else {
            console.log(`     Object not found: ${objName}`);
          }
          continue;
        }

        console.log(`  📖 ${objName} (${objTypeText})`);
        if (description) {
          console.log(`     ${description}`);
        }

        // Display source code for classes, interfaces, CDS views, programs/source includes, and STOB
        const source = obj.SOURCE || obj.source || '';
        const sections = obj.SECTIONS || obj.sections || [];

        if (sections.length > 0) {
          // --full mode: render all sections.
          // --full --lines adds dual line numbers per line for debugging.
          console.log('');
          for (const section of sections) {
            const suffix = section.SUFFIX || section.suffix || '';
            const methodName = section.METHOD_NAME || section.method_name || '';
            const file = section.FILE || section.file || '';
            const lines = section.LINES || section.lines || [];
            const isCmSection = suffix.startsWith('CM') && methodName;
            const isFugrFmSection = !isCmSection && !!methodName;

            if (linesMode) {
              // --full --lines: dual line numbers (G [N]) for debugging
              const globalStart = section.globalStart || 0;

              // Map abapGit file suffix to the --include flag value used in debug set hints.
              // User-facing names mirror the abapGit file suffixes (.clas.<name>.abap).
              // Verified by live ADT testing: /includes/<adtType> endpoint accepts BPs.
              const INCLUDE_FLAG_VALUE = {
                testclasses:  'testclasses',
                locals_imp:   'locals_imp',
                locals_def:   'locals_def',
              };
              const includeFlag = INCLUDE_FLAG_VALUE[file] || null;

              if (isCmSection) {
                let bpHint;
                if (globalStart) {
                  const execOffset = findFirstExecutableLine(lines);
                  const execLine = globalStart + execOffset;
                  bpHint = `debug set --objects ${objName}:${execLine}`;
                } else {
                  bpHint = `debug set --objects ${objName}:<global_line>`;
                }
                console.log(`  * ---- Method: ${methodName} (${suffix}) — breakpoint: ${bpHint} ----`);
              } else if (isFugrFmSection) {
                // Find first executable line: skip FUNCTION header, comments, blanks,
                // and declaration blocks (DATA:, CONSTANTS:, TYPES:, etc.)
                const declPat = /^\s*(data|final|types|constants|class-data)[\s:(]/i;
                let firstExecLine = 1;
                let inDecl = false;
                for (let li = 0; li < lines.length; li++) {
                  const t = lines[li].trim();
                  if (inDecl) {
                    if (t.endsWith('.')) inDecl = false;
                    continue;
                  }
                  if (!t) continue;
                  if (/^\*/.test(t)) continue;
                  if (/^"/.test(t)) continue;
                  if (/^function\s+/i.test(t)) continue;
                  if (declPat.test(t)) {
                    if (!t.endsWith('.')) inDecl = true;
                    continue;
                  }
                  firstExecLine = li + 1;
                  break;
                }
                const bpHint = `debug set --objects ${suffix}:${firstExecLine}`;
                console.log(`  * ---- FM: ${methodName} (${suffix}) — breakpoint: ${bpHint} ----`);
              } else if (file) {
                console.log(`  * ---- Section: ${section.DESCRIPTION || section.description} (from .clas.${file}.abap) ----`);
              } else if (suffix) {
                // For program source sections, emit a breakpoint hint at the first executable line.
                const isProgSection = suffix === 'PROG' || suffix === 'prog';
                if (isProgSection) {
                  const execOffset = findFirstExecutableLine(lines);
                  const execLine = execOffset + 1; // 1-based
                  const bpHint = `debug set --objects ${objName}:${execLine}`;
                  console.log(`  * ---- Section: ${section.DESCRIPTION || section.description} (${suffix}) — breakpoint: ${bpHint} ----`);
                } else {
                  console.log(`  * ---- Section: ${section.DESCRIPTION || section.description} (${suffix}) ----`);
                }
              }

              let includeRelLine = 0;
              // Track when we're inside a METHOD block in a sub-include section
              // so we can emit a breakpoint hint at the first executable line.
              let inSubMethod = false;
              let subMethodName = '';
              let subMethodStartLine = 0; // 1-based line of METHOD statement
              for (const codeLine of lines) {
                includeRelLine++;
                const globalLine = globalStart ? globalStart + includeRelLine - 1 : 0;
                if (isCmSection) {
                  const gStr = globalLine ? String(globalLine).padStart(4) : '    ';
                  const iStr = String(includeRelLine).padStart(3);
                  console.log(`  ${gStr} [${iStr}]  ${codeLine}`);
                } else if (isFugrFmSection) {
                  // FM include: line numbers are include-relative = ADT line numbers
                  const lStr = String(includeRelLine).padStart(4);
                  console.log(`  ${lStr}  ${codeLine}`);
                } else {
                  // For sub-include sections with a known ADT include type,
                  // detect METHOD..ENDMETHOD blocks and emit breakpoint hints.
                  if (includeFlag) {
                    const trimmed = codeLine.trim();
                    if (!inSubMethod && /^method\s+/i.test(trimmed)) {
                      // Entering a new method — find first executable line offset
                      // by scanning ahead from this line
                      const mName = (trimmed.match(/^method\s+([\w~]+)/i) || [])[1] || '';
                      // Collect lines from this METHOD onwards to find exec offset
                      const remainingLines = lines.slice(includeRelLine - 1); // 0-based from current
                      const execOffset = findFirstExecutableLine(remainingLines);
                      const execLine = includeRelLine + execOffset; // section-local line
                      const bpHint = `debug set --objects ${objName}:${execLine} --include ${includeFlag}`;
                      console.log(`  * ---- Method: ${mName.toUpperCase()} — breakpoint: ${bpHint} ----`);
                      inSubMethod = true;
                      subMethodName = mName;
                    } else if (inSubMethod && /^endmethod\s*\./i.test(codeLine.trim())) {
                      inSubMethod = false;
                    }
                  }
                  const lStr = String(includeRelLine).padStart(4);
                  console.log(`  ${lStr}  ${codeLine}`);
                }
              }
            } else {
              // --full (no --lines): clean source, no line numbers
              if (isCmSection) {
                console.log(`  * ---- Method: ${methodName} (${suffix}) ----`);
              } else if (isFugrFmSection) {
                console.log(`  * ---- FM: ${methodName} (${suffix}) ----`);
              } else if (file) {
                console.log(`  * ---- Section: ${section.DESCRIPTION || section.description} (from .clas.${file}.abap) ----`);
              } else if (suffix) {
                console.log(`  * ---- Section: ${section.DESCRIPTION || section.description} (${suffix}) ----`);
              }
              for (const codeLine of lines) {
                console.log(`  ${codeLine}`);
              }
            }
          }
        } else if (source && (objType === 'INTF' || objType === 'Interface' || objType === 'CLAS' || objType === 'Class' || objType === 'DDLS' || objType === 'CDS View' || objType === 'PROG' || objType === 'Program' || objType === 'STOB' || objType === 'Structured Object')) {
          console.log('');
          // Replace escaped newlines with actual newlines and display
          const displaySource = source.replace(/\\n/g, '\n');
          const lines = displaySource.split('\n');
          for (const line of lines) {
            console.log(`  ${line}`);
          }
        }

        if (methods.length > 0) {
          console.log(`     Methods: ${methods.length}`);
          for (const method of methods.slice(0, 5)) {
            const name = method.NAME || method.name || '';
            const visibility = method.VISIBILITY || method.visibility || '';
            console.log(`       - ${visibility} ${name}`);
          }
          if (methods.length > 5) {
            console.log(`       ... and ${methods.length - 5} more`);
          }
        }

        if (components.length > 0) {
          // Check if this is a data element (DTEL) - show domain info in property format
          if (objType === 'DTEL' || objType === 'Data Element') {
            const propWidth = 18;
            const valueWidth = 40;

            // Build separator with corners
            const sep = '┌' + '─'.repeat(propWidth + 2) + '┬' + '─'.repeat(valueWidth + 2) + '┐';
            const mid = '├' + '─'.repeat(propWidth + 2) + '┼' + '─'.repeat(valueWidth + 2) + '┤';
            const end = '└' + '─'.repeat(propWidth + 2) + '┴' + '─'.repeat(valueWidth + 2) + '┘';

            // Helper to build row
            const buildPropRow = (property, value) => {
              return '│ ' + String(property || '').padEnd(propWidth) + ' │ ' +
                     String(value || '').substring(0, valueWidth).padEnd(valueWidth) + ' │';
            };

            console.log(`  DATA ELEMENT ${objName}:`);
            console.log(sep);
            console.log(buildPropRow('Property', 'Value'));
            console.log(mid);

            // Collect properties from top-level fields and components
            const domain = obj.DOMAIN || obj.domain || '';
            const domainType = obj.DOMAIN_TYPE || obj.domain_type || '';
            const domainLength = obj.DOMAIN_LENGTH || obj.domain_length || 0;
            const domainDecimals = obj.DOMAIN_DECIMALS || obj.domain_decimals || 0;
            const description = obj.DESCRIPTION || obj.description || '';

            if (domainType) {
              console.log(buildPropRow('Data Type', domainType));
            }
            if (domainLength) {
              console.log(buildPropRow('Length', String(domainLength)));
            }
            if (domainDecimals) {
              console.log(buildPropRow('Decimals', String(domainDecimals)));
            }
            if (description) {
              console.log(buildPropRow('Description', description));
            }
            if (domain) {
              console.log(buildPropRow('Domain', domain));
            }

            console.log(end);
          } else if (objType === 'TTYP' || objType === 'Table Type') {
            // Show TTYP details as simple text lines
            console.log('');
            for (const comp of components) {
              const desc = comp.DESCRIPTION || comp.description || '';
              if (desc) {
                console.log(`   ${desc}`);
              }
            }
          } else {
            // Build table display for TABL/STRU with Data Element and Description
            const colWidths = {
              field: 16,      // Max field name length
              key: 3,
              type: 8,
              length: 8,
              dataelement: 30, // Max data element name length
              description: 60, // Max field description length
            };

            // Helper to truncate with ellipsis if needed
            const truncate = (str, maxLen) => {
              const s = String(str || '');
              if (s.length <= maxLen) return s;
              return s.substring(0, maxLen - 1) + '…';
            };

            // Helper to build row
            const buildRow = (field, key, type, length, dataelement, description) => {
              return '  | ' + truncate(field, colWidths.field).padEnd(colWidths.field) + ' | ' + String(key || '').padEnd(colWidths.key) + ' | ' + truncate(type, colWidths.type).padEnd(colWidths.type) + ' | ' + String(length || '').padStart(colWidths.length) + ' | ' + truncate(dataelement, colWidths.dataelement).padEnd(colWidths.dataelement) + ' | ' + truncate(description, colWidths.description).padEnd(colWidths.description) + ' |';
            };

            // Build separator line (matches row structure with | at ends and + between columns)
            const sep = '  |' + '-'.repeat(colWidths.field + 2) + '+' +
                        '-'.repeat(colWidths.key + 2) + '+' +
                        '-'.repeat(colWidths.type + 2) + '+' +
                        '-'.repeat(colWidths.length + 2) + '+' +
                        '-'.repeat(colWidths.dataelement + 2) + '+' +
                        '-'.repeat(colWidths.description + 2) + '|';

            // Header
            console.log(`  TABLE ${objName}:`);
            console.log(sep);
            console.log(buildRow('Field', 'Key', 'Type', 'Length', 'Data Elem', 'Description'));
            console.log(sep);

            // Rows
            for (const comp of components) {
              const key = comp.KEY || comp.key || false ? 'X' : '';
              const dataelement = comp.DATAELEMENT || comp.dataelement || '';
              const description = comp.DESCRIPTION || comp.description || '';
              console.log(buildRow(comp.FIELD || comp.field, key, comp.TYPE || comp.type, comp.LENGTH || comp.length, dataelement, description));
            }

            console.log(sep);
          }
        }

        console.log('');
      }
    }
  }
};
