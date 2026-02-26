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
          classFilesMap.set(objName, { main: null, locals_def: null, locals_imp: null });
        }
        const classFiles = classFilesMap.get(objName);
        classFiles[fileKind] = source;
      } else {
        objects.push({
          type: objType,
          name: objName,
          source: source
        });
      }
    }

    // Add class objects with their local files
    for (const [className, files] of classFilesMap) {
      // Try to auto-detect local files if only main file provided
      if (files.main && !files.locals_def && !files.locals_imp) {
        // Look for companion local files in the same directory
        const mainFile = syntaxFiles.find(f => {
          const bn = pathModule.basename(f).toUpperCase();
          return bn.startsWith(className) && bn.includes('.CLAS.ABAP');
        });
        if (mainFile) {
          const dir = pathModule.dirname(mainFile);
          const defFile = pathModule.join(dir, `${className.toLowerCase()}.clas.locals_def.abap`);
          const impFile = pathModule.join(dir, `${className.toLowerCase()}.clas.locals_imp.abap`);
          if (fs.existsSync(defFile)) {
            files.locals_def = fs.readFileSync(defFile, 'utf8');
            if (!jsonOutput) console.log(`  Auto-detected: ${pathModule.basename(defFile)}`);
          }
          if (fs.existsSync(impFile)) {
            files.locals_imp = fs.readFileSync(impFile, 'utf8');
            if (!jsonOutput) console.log(`  Auto-detected: ${pathModule.basename(impFile)}`);
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
        objects.push(obj);
      } else {
        console.error(`  Warning: No main class file for ${className}, skipping local files`);
      }
    }

    if (objects.length === 0) {
      console.error('  No valid files to check');
      process.exit(1);
    }

    // Send request
    const data = {
      objects: objects,
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
      for (const res of results) {
        const objSuccess = res.SUCCESS !== undefined ? res.SUCCESS : res.success;
        const objType = res.OBJECT_TYPE || res.object_type || 'UNKNOWN';
        const objName = res.OBJECT_NAME || res.object_name || 'UNKNOWN';
        const errorCount = res.ERROR_COUNT || res.error_count || 0;
        const errors = res.ERRORS || res.errors || [];
        const warnings = res.WARNINGS || res.warnings || [];
        const objMessage = res.MESSAGE || res.message || '';

        if (objSuccess) {
          console.log(`✅ ${objType} ${objName} - Syntax check passed`);
          if (warnings.length > 0) {
            console.log(`   (${warnings.length} warning(s))`);
          }
        } else {
          console.log(`❌ ${objType} ${objName} - Syntax check failed (${errorCount} error(s))`);
          console.log('');
          console.log('Errors:');
          console.log('─'.repeat(60));

          for (const err of errors) {
            const line = err.LINE || err.line || '?';
            const column = err.COLUMN || err.column || '';
            const text = err.TEXT || err.text || 'Unknown error';
            const methodName = err.METHOD_NAME || err.method_name || '';

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
