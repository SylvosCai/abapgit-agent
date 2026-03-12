/**
 * View command - View ABAP object definitions
 */

module.exports = {
  name: 'view',
  description: 'View ABAP object definitions from ABAP system',
  requiresAbapConfig: true,
  requiresVersionCheck: true,

  async execute(args, context) {
    const { loadConfig, AbapHttp } = context;

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
          // --full mode: render with dual line numbers
          console.log('');
          let globalLine = 0;
          for (const section of sections) {
            const suffix = section.SUFFIX || section.suffix || '';
            const methodName = section.METHOD_NAME || section.method_name || '';
            const file = section.FILE || section.file || '';
            const lines = section.LINES || section.lines || [];
            const isCmSection = suffix.startsWith('CM') && methodName;

            if (isCmSection) {
              // Method header comment — no line number increment for the comment itself
              const startGlobal = globalLine + 1;
              console.log(`  * ---- Method: ${methodName} (${suffix}) [include line: 1 = global line ${startGlobal}] ----`);
            } else if (file) {
              console.log(`  * ---- Section: ${section.DESCRIPTION || section.description} (from .clas.${file}.abap) ----`);
            }

            let includeRelLine = 0;
            for (const codeLine of lines) {
              globalLine++;
              includeRelLine++;
              const gStr = String(globalLine).padStart(4);
              if (isCmSection) {
                const iStr = String(includeRelLine).padStart(3);
                console.log(`  ${gStr}  [${iStr}]  ${codeLine}`);
              } else {
                console.log(`  ${gStr}    ${codeLine}`);
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
