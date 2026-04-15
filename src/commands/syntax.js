/**
 * Syntax command - Check syntax of ABAP source files directly (without pull/activation)
 */

const pathModule = require('path');
const fs = require('fs');

module.exports = {
  name: 'syntax',
  description: 'Check syntax of ABAP source files without pull/activation',
  requiresAbapConfig: true,
  requiresVersionCheck: true,

  async execute(args, context) {
    const { loadConfig, AbapHttp } = context;

    if (args.includes('--help') || args.includes('-h')) {
      console.log(`
Usage:
  abapgit-agent syntax --files <file1>,<file2>,... [--cloud] [--json]

Description:
  Check syntax of local ABAP source files WITHOUT pulling or activating them.
  Reads source from local files and checks directly in the ABAP system.

Parameters:
  --files <file1,...>  Comma-separated ABAP source files (required). Accepts CLAS, INTF, PROG, ENHO.
  --cloud              Use ABAP Cloud (BTP) stricter syntax check.
  --json               Output as JSON.

Examples:
  abapgit-agent syntax --files src/zcl_my_class.clas.abap
  abapgit-agent syntax --files src/zcl_foo.clas.abap,src/zif_foo.intf.abap
  abapgit-agent syntax --files src/zmy_prog.prog.abap --cloud
`);
      return;
    }

    const filesArgIndex = args.indexOf('--files');
    if (filesArgIndex === -1 || filesArgIndex + 1 >= args.length) {
      console.error('Error: --files parameter required');
      console.error('Usage: abapgit-agent syntax --files <file1>,<file2>,... [--cloud] [--json]');
      console.error('');
      console.error('Options:');
      console.error('  --cloud         Use ABAP Cloud syntax check (stricter)');
      console.error('  --json          Output raw JSON');
      console.error('');
      console.error('Examples:');
      console.error('  abapgit-agent syntax --files src/zcl_my_class.clas.abap');
      console.error('  abapgit-agent syntax --files src/zcl_my_class.clas.abap --cloud');
      process.exit(1);
    }

    const syntaxFiles = args[filesArgIndex + 1].split(',').map(f => f.trim());
    const cloudMode = args.includes('--cloud');
    const jsonOutput = args.includes('--json');

    if (!jsonOutput) {
      console.log(`\n  Syntax check for ${syntaxFiles.length} file(s)`);
      if (cloudMode) {
        console.log('  Mode: ABAP Cloud');
      }
      console.log('');
    }

    const config = loadConfig();
    const http = new AbapHttp(config);
    const csrfToken = await http.fetchCsrfToken();

    // Build objects array from files
    // Group class files together (main + locals)
    const classFilesMap = new Map(); // className -> { main, locals_def, locals_imp }
    const fugrGroupMap = new Map();  // groupName -> { dir, fmFiles: Map<fmName, source> }
    const progIncludeMap = new Map(); // includeName -> { filePath, source }
    const objects = [];

    for (const file of syntaxFiles) {
      const baseName = pathModule.basename(file);
      let objType = 'PROG';
      let objName = baseName.toUpperCase();
      let fileKind = 'main'; // main, locals_def, locals_imp

      // Parse file type from extension
      if (baseName.includes('.clas.locals_def.')) {
        objType = 'CLAS';
        objName = baseName.split('.')[0].toUpperCase();
        fileKind = 'locals_def';
      } else if (baseName.includes('.clas.locals_imp.')) {
        objType = 'CLAS';
        objName = baseName.split('.')[0].toUpperCase();
        fileKind = 'locals_imp';
      } else if (baseName.includes('.clas.testclasses.')) {
        objType = 'CLAS';
        objName = baseName.split('.')[0].toUpperCase();
        fileKind = 'locals_imp';  // Test classes are implementations
      } else if (baseName.includes('.clas.')) {
        objType = 'CLAS';
        objName = baseName.split('.')[0].toUpperCase();
        fileKind = 'main';
      } else if (baseName.includes('.intf.')) {
        objType = 'INTF';
        objName = baseName.split('.')[0].toUpperCase();
      } else if (baseName.includes('.prog.')) {
        objType = 'PROG';
        objName = baseName.split('.')[0].toUpperCase();
      } else if (/\.enho\.[0-9a-f]{8}\.abap$/i.test(baseName)) {
        objType = 'ENHO';
        objName = baseName.split('.')[0].toUpperCase();
      } else if (baseName.includes('.ddls.asddls')) {
        objType = 'DDLS';
        objName = baseName.split('.')[0].toUpperCase();
        fileKind = 'main';
      } else if (baseName.includes('.fugr.')) {
        objType = 'FUGR';
        const parts = baseName.split('.');   // e.g. ['zmy_fugr', 'fugr', 'zmy_my_function', 'abap']
        objName = parts[0].toUpperCase();    // group name e.g. 'ZMY_FUGR'
        const includeFile = parts[2] || ''; // e.g. 'zmy_my_function', 'lzmy_fugrtop', 'saplzmy_fugr'
        const groupLower = parts[0].toLowerCase();
        const isTopInclude = new RegExp(`^l${groupLower}top$`, 'i').test(includeFile);
        const isUInclude   = new RegExp(`^l${groupLower}u\\d+$`, 'i').test(includeFile);
        const isSapl       = includeFile.toLowerCase().startsWith('sapl');
        const isFm = !isTopInclude && !isUInclude && !isSapl && includeFile !== '';

        // Read source and store in fugrGroupMap
        const filePath = pathModule.resolve(file);
        if (!fs.existsSync(filePath)) {
          console.error(`  Error: File not found: ${file}`);
          continue;
        }
        const source = fs.readFileSync(filePath, 'utf8');
        const dir = pathModule.dirname(filePath);

        if (!fugrGroupMap.has(objName)) {
          fugrGroupMap.set(objName, { dir, fmFiles: new Map() });
        }
        if (isFm) {
          const fmName = includeFile.toUpperCase();
          fugrGroupMap.get(objName).fmFiles.set(fmName, source);
        }
        // Skip adding to objects here — handled after auto-detection below
        continue;
      }

      // Read source from file
      const filePath = pathModule.resolve(file);
      if (!fs.existsSync(filePath)) {
        console.error(`  Error: File not found: ${file}`);
        continue;
      }

      const source = fs.readFileSync(filePath, 'utf8');

      // For class files, group them together
      if (objType === 'CLAS') {
        if (!classFilesMap.has(objName)) {
          classFilesMap.set(objName, { main: null, locals_def: null, locals_imp: null, testclasses: null });
        }
        const classFiles = classFilesMap.get(objName);
        // For testclasses, store in testclasses field (not locals_imp)
        if (fileKind === 'locals_imp' && baseName.includes('.testclasses.')) {
          classFiles.testclasses = source;
        } else {
          classFiles[fileKind] = source;
        }
      } else {
        const obj = {
          type: objType,
          name: objName,
          source: source
        };

        // Read FIXPT and SUBC from XML metadata for INTF and PROG
        if (objType === 'INTF' || objType === 'PROG') {
          const dir = pathModule.dirname(filePath);
          let xmlFile;
          if (objType === 'INTF') {
            xmlFile = pathModule.join(dir, `${objName.toLowerCase()}.intf.xml`);
          } else if (objType === 'PROG') {
            xmlFile = pathModule.join(dir, `${objName.toLowerCase()}.prog.xml`);
          }
          if (xmlFile && fs.existsSync(xmlFile)) {
            const xmlContent = fs.readFileSync(xmlFile, 'utf8');
            const fixptMatch = xmlContent.match(/<FIXPT>([^<]+)<\/FIXPT>/);
            if (fixptMatch && fixptMatch[1] === 'X') {
              obj.fixpt = 'X';
            } else {
              obj.fixpt = '';
            }
            if (objType === 'PROG') {
              const subcMatch = xmlContent.match(/<SUBC>([^<]+)<\/SUBC>/);
              obj.subc = subcMatch ? subcMatch[1] : '';

              // INCLUDE programs: stash for parent auto-detection instead of pushing directly
              if (obj.subc === 'I') {
                progIncludeMap.set(objName, { filePath, source });
                continue; // skip objects.push — handled after auto-detection below
              }
            }
          }
        }

        objects.push(obj);
      }
    }

    // Add class objects with their local files
    for (const [className, files] of classFilesMap) {
      // Try to auto-detect companion files if only one type is provided
      if (files.main && !files.locals_def && !files.locals_imp && !files.testclasses) {
        // Main file provided - look for companion local files
        const mainFile = syntaxFiles.find(f => {
          const bn = pathModule.basename(f).toUpperCase();
          return bn.startsWith(className) && bn.includes('.CLAS.ABAP') && !bn.includes('LOCALS') && !bn.includes('TESTCLASSES');
        });
        if (mainFile) {
          const dir = pathModule.dirname(mainFile);
          const defFile = pathModule.join(dir, `${className.toLowerCase()}.clas.locals_def.abap`);
          const impFile = pathModule.join(dir, `${className.toLowerCase()}.clas.locals_imp.abap`);
          const testFile = pathModule.join(dir, `${className.toLowerCase()}.clas.testclasses.abap`);
          if (fs.existsSync(defFile)) {
            files.locals_def = fs.readFileSync(defFile, 'utf8');
            if (!jsonOutput) console.log(`  Auto-detected: ${pathModule.basename(defFile)}`);
          }
          if (fs.existsSync(impFile)) {
            files.locals_imp = fs.readFileSync(impFile, 'utf8');
            if (!jsonOutput) console.log(`  Auto-detected: ${pathModule.basename(impFile)}`);
          }
          if (fs.existsSync(testFile)) {
            files.testclasses = fs.readFileSync(testFile, 'utf8');
            if (!jsonOutput) console.log(`  Auto-detected: ${pathModule.basename(testFile)}`);
          }
        }
      } else if (!files.main && (files.locals_def || files.locals_imp || files.testclasses)) {
        // Any local file provided - look for main class file and other companions
        const localFile = syntaxFiles.find(f => {
          const bn = pathModule.basename(f).toUpperCase();
          return bn.startsWith(className) && (bn.includes('.LOCALS_') || bn.includes('.TESTCLASSES.'));
        });
        if (localFile) {
          const dir = pathModule.dirname(localFile);
          const mainFile = pathModule.join(dir, `${className.toLowerCase()}.clas.abap`);
          const defFile = pathModule.join(dir, `${className.toLowerCase()}.clas.locals_def.abap`);
          const impFile = pathModule.join(dir, `${className.toLowerCase()}.clas.locals_imp.abap`);
          const testFile = pathModule.join(dir, `${className.toLowerCase()}.clas.testclasses.abap`);

          if (fs.existsSync(mainFile)) {
            files.main = fs.readFileSync(mainFile, 'utf8');
            if (!jsonOutput) console.log(`  Auto-detected: ${pathModule.basename(mainFile)}`);
          }
          // Also auto-detect other companion files
          if (!files.locals_def && fs.existsSync(defFile)) {
            files.locals_def = fs.readFileSync(defFile, 'utf8');
            if (!jsonOutput) console.log(`  Auto-detected: ${pathModule.basename(defFile)}`);
          }
          if (!files.locals_imp && fs.existsSync(impFile)) {
            files.locals_imp = fs.readFileSync(impFile, 'utf8');
            if (!jsonOutput) console.log(`  Auto-detected: ${pathModule.basename(impFile)}`);
          }
          if (!files.testclasses && fs.existsSync(testFile)) {
            files.testclasses = fs.readFileSync(testFile, 'utf8');
            if (!jsonOutput) console.log(`  Auto-detected: ${pathModule.basename(testFile)}`);
          }
        }
      }

      if (files.main) {
        const obj = {
          type: 'CLAS',
          name: className,
          source: files.main
        };
        if (files.locals_def) obj.locals_def = files.locals_def;
        if (files.locals_imp) obj.locals_imp = files.locals_imp;
        if (files.testclasses) obj.testclasses = files.testclasses;

        // Read FIXPT from XML metadata
        const mainFile = syntaxFiles.find(f => {
          const bn = pathModule.basename(f).toUpperCase();
          return bn.startsWith(className) && bn.includes('.CLAS.ABAP') && !bn.includes('LOCALS') && !bn.includes('TESTCLASSES');
        });
        if (mainFile) {
          const dir = pathModule.dirname(mainFile);
          const xmlFile = pathModule.join(dir, `${className.toLowerCase()}.clas.xml`);
          if (fs.existsSync(xmlFile)) {
            const xmlContent = fs.readFileSync(xmlFile, 'utf8');
            // Simple regex to extract FIXPT value
            const fixptMatch = xmlContent.match(/<FIXPT>([^<]+)<\/FIXPT>/);
            if (fixptMatch && fixptMatch[1] === 'X') {
              obj.fixpt = 'X';
            } else {
              // No FIXPT tag means FIXPT=false (blank)
              obj.fixpt = '';
            }
          }
        }

        objects.push(obj);
      } else {
        console.error(`  Warning: No main class file for ${className}, skipping local files`);
      }
    }

    // Helper: read FIXPT from SAPL XML for a function group
    function readFugrFixpt(dir, groupName) {
      const xmlFile = pathModule.join(dir, `${groupName.toLowerCase()}.fugr.sapl${groupName.toLowerCase()}.xml`);
      if (fs.existsSync(xmlFile)) {
        const xmlContent = fs.readFileSync(xmlFile, 'utf8');
        const fixptMatch = xmlContent.match(/<FIXPT>([^<]+)<\/FIXPT>/);
        if (fixptMatch && fixptMatch[1] === 'X') return 'X';
      }
      return '';
    }

    // Helper: classify a FUGR include filename — returns fm name (uppercase) or null if not an FM file
    function getFugrFmName(includeFile, groupLower) {
      if (!includeFile) return null;
      const isTopInclude = new RegExp(`^l${groupLower}top$`, 'i').test(includeFile);
      const isUInclude   = new RegExp(`^l${groupLower}u\\d+$`, 'i').test(includeFile);
      const isSapl       = includeFile.toLowerCase().startsWith('sapl');
      if (isTopInclude || isUInclude || isSapl) return null;
      return includeFile.toUpperCase();
    }

    // Auto-detect all FM files for each function group and build objects
    for (const [groupName, groupData] of fugrGroupMap) {
      const { dir } = groupData;
      const groupLower = groupName.toLowerCase();
      const prefix = `${groupLower}.fugr.`;

      // Scan directory for all FUGR files belonging to this group
      let allFiles;
      try {
        allFiles = fs.readdirSync(dir);
      } catch (e) {
        allFiles = [];
      }
      for (const f of allFiles) {
        if (!f.toLowerCase().startsWith(prefix) || !f.toLowerCase().endsWith('.abap')) continue;
        const parts = f.split('.');
        const includeFile = parts[2] || '';
        const fmName = getFugrFmName(includeFile, groupLower);
        if (fmName && !groupData.fmFiles.has(fmName)) {
          groupData.fmFiles.set(fmName, fs.readFileSync(pathModule.join(dir, f), 'utf8'));
          if (!jsonOutput) console.log(`  Auto-detected: ${f}`);
        }
      }

      if (groupData.fmFiles.size === 0) {
        if (!jsonOutput) console.error(`  Warning: No FM source files found for FUGR ${groupName}`);
        continue;
      }

      const fixpt = readFugrFixpt(dir, groupName);

      // Add one object per FM
      for (const [fmName, source] of groupData.fmFiles) {
        objects.push({
          type: 'FUGR',
          name: groupName,
          source: source,
          fugr_include_name: fmName,
          fixpt: fixpt
        });
      }
    }

    // Helper: read SUBC from a .prog.xml file
    function readProgSubc(xmlPath) {
      if (!fs.existsSync(xmlPath)) return '';
      const xml = fs.readFileSync(xmlPath, 'utf8');
      const m = xml.match(/<SUBC>([^<]+)<\/SUBC>/);
      return m ? m[1] : '';
    }

    // Helper: read FIXPT from a .prog.xml file
    function readProgFixpt(xmlPath) {
      if (!fs.existsSync(xmlPath)) return '';
      const xml = fs.readFileSync(xmlPath, 'utf8');
      const m = xml.match(/<FIXPT>([^<]+)<\/FIXPT>/);
      return (m && m[1] === 'X') ? 'X' : '';
    }

    // Auto-detect parent program for INCLUDE files
    // Scan the same directory for .prog.abap files that contain INCLUDE <name>.
    // Assemble the parent source with the INCLUDE source substituted in-place.
    for (const [includeName, includeData] of progIncludeMap) {
      const includeDir = pathModule.dirname(includeData.filePath);
      let parentFound = false;

      let dirEntries;
      try {
        dirEntries = fs.readdirSync(includeDir);
      } catch (e) {
        dirEntries = [];
      }

      for (const entry of dirEntries) {
        if (!entry.toLowerCase().endsWith('.prog.abap')) continue;
        const candidatePath = pathModule.join(includeDir, entry);
        if (candidatePath === includeData.filePath) continue; // skip itself

        // Only consider executable/non-include programs as parents
        const candidateName = entry.split('.')[0].toUpperCase();
        const candidateXml = pathModule.join(includeDir, `${candidateName.toLowerCase()}.prog.xml`);
        const candidateSubc = readProgSubc(candidateXml);
        if (candidateSubc === 'I') continue; // another INCLUDE — skip

        const parentSource = fs.readFileSync(candidatePath, 'utf8');
        const parentLines = parentSource.split('\n');

        // Find `INCLUDE <includeName>.` line (case-insensitive)
        const includeRegex = new RegExp(`^\\s*INCLUDE\\s+${includeName}\\s*\\.`, 'i');
        const includeLineIdx = parentLines.findIndex(l => includeRegex.test(l));
        if (includeLineIdx === -1) continue;

        // Found parent — assemble: replace INCLUDE statement with include's source lines
        const includeLines = includeData.source.split('\n');
        const assembled = [
          ...parentLines.slice(0, includeLineIdx),
          ...includeLines,
          ...parentLines.slice(includeLineIdx + 1)
        ];
        const assembledSource = assembled.join('\n');

        if (!jsonOutput) {
          console.log(`  Auto-detected parent: ${entry} (contains INCLUDE ${includeName}.)`);
        }

        objects.push({
          type: 'PROG',
          name: candidateName,
          source: assembledSource,
          subc: candidateSubc || '1',
          fixpt: readProgFixpt(candidateXml),
          // offset into assembled source where include lines start (1-based)
          include_offset: includeLineIdx + 1,
          include_line_count: includeLines.length,
          // carry include name for error line mapping
          include_name: includeName
        });

        parentFound = true;
        break; // use first matching parent
      }

      if (!parentFound) {
        if (!jsonOutput) {
          console.error(`  Warning: No parent program found for INCLUDE ${includeName} — cannot syntax-check standalone`);
        }
      }
    }

    if (objects.length === 0) {
      console.error('  No valid files to check');
      process.exit(1);
    }
    const data = {
      objects: objects.map(o => {
        // Strip client-side INCLUDE tracking fields before sending to ABAP
        const { include_offset, include_line_count, include_name, ...abapObj } = o; // eslint-disable-line no-unused-vars
        return abapObj;
      }),
      uccheck: cloudMode ? '5' : 'X'
    };

    const result = await http.post('/sap/bc/z_abapgit_agent/syntax', data, { csrfToken });

    // Handle response
    const success = result.SUCCESS !== undefined ? result.SUCCESS : result.success;
    const results = result.RESULTS || result.results || [];
    const message = result.MESSAGE || result.message || '';

    if (jsonOutput) {
      console.log(JSON.stringify(result, null, 2));
    } else {
      // Display results for each object
      for (let i = 0; i < results.length; i++) {
        const res = results[i];
        const objSuccess = res.SUCCESS !== undefined ? res.SUCCESS : res.success;
        const objType = res.OBJECT_TYPE || res.object_type || 'UNKNOWN';
        const objName = res.OBJECT_NAME || res.object_name || 'UNKNOWN';
        const errorCount = res.ERROR_COUNT || res.error_count || 0;
        const errors = res.ERRORS || res.errors || [];
        const warnings = res.WARNINGS || res.warnings || [];
        const objMessage = res.MESSAGE || res.message || '';

        // For FUGR: show which FM was checked alongside the group name
        const sentObj = objects[i] || {};
        const fugrFmLabel = (objType === 'FUGR' && sentObj.fugr_include_name)
          ? ` (${sentObj.fugr_include_name})` : '';

        // For INCLUDE-backed checks: determine display label and line remapping
        const includeLabel = sentObj.include_name
          ? ` (checking via parent ${objName})` : '';

        if (objSuccess) {
          if (sentObj.include_name) {
            console.log(`✅ PROG ${sentObj.include_name} - Syntax check passed${includeLabel}`);
          } else {
            console.log(`✅ ${objType} ${objName}${fugrFmLabel} - Syntax check passed`);
          }
          if (warnings.length > 0) {
            console.log(`   (${warnings.length} warning(s))`);
          }
        } else {
          if (sentObj.include_name) {
            console.log(`❌ PROG ${sentObj.include_name} - Syntax check failed (${errorCount} error(s))${includeLabel}`);
          } else {
            console.log(`❌ ${objType} ${objName}${fugrFmLabel} - Syntax check failed (${errorCount} error(s))`);
          }
          console.log('');
          console.log('Errors:');
          console.log('─'.repeat(60));

          for (const err of errors) {
            let line = err.LINE || err.line || '?';
            const column = err.COLUMN || err.column || '';
            const text = err.TEXT || err.text || 'Unknown error';
            const methodName = err.METHOD_NAME || err.method_name || '';
            const include = err.INCLUDE || err.include || '';

            // Remap line numbers for INCLUDE-backed checks
            let remappedFile = null;
            if (sentObj.include_name && typeof line === 'number') {
              const offset = sentObj.include_offset || 0;
              const count = sentObj.include_line_count || 0;
              if (line >= offset && line < offset + count) {
                // Error is inside the INCLUDE — remap to include-relative line
                line = line - offset + 1;
                remappedFile = `${sentObj.include_name.toLowerCase()}.prog.abap`;
              } else {
                // Error is in the parent program context (e.g. header line)
                remappedFile = `${objName.toLowerCase()}.prog.abap (parent)`;
              }
            }

            // Display which file/include the error is in
            if (remappedFile) {
              // INCLUDE-backed check: show the actual source file the error maps to
              console.log(`  In: ${remappedFile}`);
            } else if (include) {
              // For FUGR: include = lowercase FM name → display as '<group>.fugr.<fm_name>.abap'
              if (objType === 'FUGR') {
                const fugrFile = `${objName.toLowerCase()}.fugr.${include}.abap`;
                console.log(`  In: Function module ${include.toUpperCase()} (${fugrFile})`);
              } else {
                const includeMap = {
                  'main': { display: 'Main class', suffix: '.clas.abap' },
                  'locals_def': { display: 'Local definitions', suffix: '.clas.locals_def.abap' },
                  'locals_imp': { display: 'Local implementations', suffix: '.clas.locals_imp.abap' },
                  'testclasses': { display: 'Test classes', suffix: '.clas.testclasses.abap' }
                };
                const includeInfo = includeMap[include] || { display: include, suffix: '' };

                // Show both display name and filename
                if (includeInfo.suffix) {
                  console.log(`  In: ${includeInfo.display} (${objName.toLowerCase()}${includeInfo.suffix})`);
                } else {
                  console.log(`  In: ${includeInfo.display}`);
                }
              }
            }
            if (methodName) {
              console.log(`  Method: ${methodName}`);
            }
            if (column) {
              console.log(`  Line ${line}, Column ${column}:`);
            } else {
              console.log(`  Line ${line}:`);
            }
            console.log(`    ${text}`);
            console.log('');
          }
        }

        // Show warnings if any
        if (warnings.length > 0) {
          console.log('');
          console.log('Warnings:');
          console.log('─'.repeat(60));
          for (const warn of warnings) {
            const line = warn.LINE || warn.line || '?';
            const text = warn.TEXT || warn.text || warn.MESSAGE || warn.message || '';
            console.log(`  Line ${line}: ${text}`);
          }
        }
        console.log('');
      }

      // Overall summary
      if (success) {
        console.log(`✅ ${message}`);
      } else {
        console.log(`❌ ${message}`);
      }
    }
  }
};
