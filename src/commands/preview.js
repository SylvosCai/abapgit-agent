/**
 * Preview command - Preview data from ABAP tables or CDS views
 */

module.exports = {
  name: 'preview',
  description: 'Preview data from ABAP tables or CDS views',
  requiresAbapConfig: true,
  requiresVersionCheck: true,

  async execute(args, context) {
    const { loadConfig, AbapHttp, validators } = context;

    if (args.includes('--help') || args.includes('-h')) {
      console.log(`
Usage:
  abapgit-agent preview --objects <obj1>,... [--type <type>] [--limit <n>] [--offset <n>]
                        [--where <condition>] [--columns <cols>] [--vertical] [--compact] [--json]

Description:
  Preview data from ABAP tables or CDS views.

Parameters:
  --objects <obj1,...>   Comma-separated table/view names (required).
  --type <type>          Object type: TABL, DDLS (auto-detected if omitted).
  --limit <n>            Maximum number of rows (default: 20).
  --offset <n>           Skip first N rows (for pagination).
  --where <condition>    WHERE clause filter (e.g. "CARRID = 'AA'").
  --columns <cols>       Comma-separated column names to display.
  --vertical             Display each row vertically (field: value).
  --compact              Compact output (no borders).
  --json                 Output as JSON.

Examples:
  abapgit-agent preview --objects SFLIGHT
  abapgit-agent preview --objects SFLIGHT --limit 5
  abapgit-agent preview --objects SFLIGHT --where "CARRID = 'AA'"
  abapgit-agent preview --objects ZC_MY_VIEW --type DDLS
  abapgit-agent preview --objects SFLIGHT --offset 10 --limit 20
`);
      return;
    }

    const objectsArgIndex = args.indexOf('--objects');
    if (objectsArgIndex === -1 || objectsArgIndex + 1 >= args.length) {
      console.error('Error: --objects parameter required');
      console.error('Usage: abapgit-agent preview --objects <table1>,<view1>,... [--type <type>] [--limit <n>] [--offset <n>] [--where <condition>] [--columns <cols>] [--vertical] [--compact] [--json]');
      console.error('Example: abapgit-agent preview --objects SFLIGHT');
      console.error('Example: abapgit-agent preview --objects ZC_MY_CDS_VIEW --type DDLS');
      console.error('Example: abapgit-agent preview --objects SFLIGHT --where "CARRID = \'AA\'"');
      console.error('Example: abapgit-agent preview --objects SFLIGHT --offset 10 --limit 20');
      process.exit(1);
    }

    const objects = args[objectsArgIndex + 1].split(',').map(o => o.trim().toUpperCase());
    const typeArg = args.indexOf('--type');
    const type = typeArg !== -1 ? args[typeArg + 1].toUpperCase() : null;
    const limitArg = args.indexOf('--limit');
    const limitRaw = limitArg !== -1 ? args[limitArg + 1] : null;
    const limitParsed = parseInt(limitRaw, 10);
    const limit = limitRaw && !limitRaw.startsWith('--') && !isNaN(limitParsed) ? Math.max(1, limitParsed) : 100;
    const offsetArg = args.indexOf('--offset');
    const offsetRaw = offsetArg !== -1 ? args[offsetArg + 1] : null;
    const offsetParsed = parseInt(offsetRaw, 10);
    const offset = offsetRaw && !offsetRaw.startsWith('--') && !isNaN(offsetParsed) ? Math.max(0, offsetParsed) : 0;
    const whereArg = args.indexOf('--where');
    const where = whereArg !== -1 ? args[whereArg + 1] : null;
    const columnsArg = args.indexOf('--columns');
    const columns = columnsArg !== -1 ? args[columnsArg + 1].split(',').map(c => c.trim().toUpperCase()) : null;
    const verticalOutput = args.includes('--vertical');
    const compactOutput = args.includes('--compact');
    const jsonOutput = args.includes('--json');

    console.log(`\n  Previewing ${objects.length} object(s)`);

    const config = loadConfig();
    const http = new AbapHttp(config);
    const csrfToken = await http.fetchCsrfToken();

    const data = {
      objects: objects,
      limit: Math.min(Math.max(1, limit), 500),
      offset: Math.max(0, offset)
    };

    if (type) {
      data.type = type;
    }

    if (where) {
      data.where = validators.convertDatesInWhereClause(where);
    }

    if (columns) {
      data.columns = columns;
    }

    const result = await http.post('/sap/bc/z_abapgit_agent/preview', data, { csrfToken });

    // Handle uppercase keys from ABAP
    const success = result.SUCCESS || result.success;
    const previewObjects = result.OBJECTS || result.objects || [];
    const message = result.MESSAGE || result.message || '';
    const error = result.ERROR || result.error;

    if (!success || error) {
      console.error(`\n  Error: ${error || 'Failed to preview objects'}`);
      return;
    }

    const pagination = result.PAGINATION || result.pagination || null;

    if (jsonOutput) {
      console.log(JSON.stringify(result, null, 2));
    } else {
      // Build pagination message
      let paginationMsg = '';
      const paginationTotal = pagination ? (pagination.TOTAL || pagination.total || 0) : 0;
      const paginationHasMore = pagination ? (pagination.HAS_MORE || pagination.has_more || false) : false;
      if (pagination && paginationTotal > 0) {
        // Handle case where offset exceeds total
        if (offset >= paginationTotal) {
          paginationMsg = ` (Offset ${offset} exceeds total ${paginationTotal})`;
        } else {
          const start = offset + 1;
          const end = Math.min(offset + limit, paginationTotal);
          paginationMsg = ` (Showing ${start}-${end} of ${paginationTotal})`;
          if (paginationHasMore) {
            paginationMsg += ` — Use --offset ${offset + limit} to see more`;
          }
        }
      }

      console.log(`\n  ${message}${paginationMsg}`);
      console.log('');

      // Track if columns were explicitly specified
      const columnsExplicit = columns !== null;

      for (let i = 0; i < previewObjects.length; i++) {
        const obj = previewObjects[i];
        const objName = obj.NAME || obj.name || `Object ${i + 1}`;
        const objType = obj.TYPE || obj.type || '';
        const objTypeText = obj.TYPE_TEXT || obj.type_text || '';
        // Parse rows - could be a JSON string or array
        let rows = obj.ROWS || obj.rows || [];
        if (typeof rows === 'string') {
          try {
            rows = JSON.parse(rows);
          } catch (e) {
            rows = [];
          }
        }
        const fields = obj.FIELDS || obj.fields || [];
        const rowCount = obj.ROW_COUNT || obj.row_count || 0;
        const totalRows = obj.TOTAL_ROWS || obj.total_rows || 0;
        const notFound = obj.NOT_FOUND || obj.not_found || false;
        const accessDenied = obj.ACCESS_DENIED || obj.access_denied || false;

        // Check if object was not found
        if (notFound) {
          console.log(`  ❌ ${objName} (${objTypeText})`);
          console.log(`     Object not found: ${objName}`);
          continue;
        }

        // Check if access denied
        if (accessDenied) {
          console.log(`  ❌ ${objName} (${objTypeText})`);
          console.log(`     Access denied to: ${objName}`);
          continue;
        }

        console.log(`  📊 Preview: ${objName} (${objTypeText})`);

        // Check for errors first
        const objError = obj.ERROR || obj.error;
        if (objError) {
          console.log(`     ❌ Error: ${objError}`);
          continue;
        }

        if (rows.length === 0) {
          console.log('     No data found');
          continue;
        }

        // Get all unique field names from all rows
        const allFields = new Set();
        rows.forEach(row => {
          Object.keys(row).forEach(key => allFields.add(key));
        });
        const allFieldNames = Array.from(allFields);

        // Display as table - use fields metadata only if --columns was explicitly specified
        let fieldNames;
        let columnsAutoSelected = false;
        if (columnsExplicit && fields && fields.length > 0) {
          // Use fields from metadata (filtered by explicit --columns)
          fieldNames = fields.map(f => f.FIELD || f.field);
        } else {
          // Use all fields - let terminal handle wrapping if needed
          // Terminal width detection is unreliable without a proper TTY
          fieldNames = allFieldNames;
        }

        // Calculate column widths - use reasonable defaults
        const colWidths = {};
        const maxColWidth = compactOutput ? 10 : 20;
        fieldNames.forEach(field => {
          let maxWidth = field.length;
          rows.forEach(row => {
            const value = String(row[field] || '');
            maxWidth = Math.max(maxWidth, value.length);
          });
          // Cap at maxColWidth (truncates both headers and data in compact mode)
          colWidths[field] = Math.min(maxWidth, maxColWidth);
        });

        // Render output - either vertical or table
        if (verticalOutput) {
          // Vertical format: each field on its own line
          rows.forEach((row, rowIndex) => {
            if (rows.length > 1) {
              console.log(`\n  Row ${rowIndex + 1}:`);
              console.log('  ' + '─'.repeat(30));
            }
            fieldNames.forEach(field => {
              const value = String(row[field] || '');
              console.log(`  ${field}: ${value}`);
            });
          });
          console.log('');
          continue;
        }

        // Build table header
        let headerLine = '  ┌';
        let separatorLine = '  ├';
        fieldNames.forEach(field => {
          const width = colWidths[field];
          headerLine += '─'.repeat(width + 2) + '┬';
          separatorLine += '─'.repeat(width + 2) + '┼';
        });
        headerLine = headerLine.slice(0, -1) + '┐';
        separatorLine = separatorLine.slice(0, -1) + '┤';

        // Build header row
        let headerRow = '  │';
        fieldNames.forEach(field => {
          const width = colWidths[field];
          let displayField = String(field);
          if (displayField.length > width) {
            displayField = displayField.slice(0, width - 3) + '...';
          }
          headerRow += ' ' + displayField.padEnd(width) + ' │';
        });

        console.log(headerLine);
        console.log(headerRow);
        console.log(separatorLine);

        // Build data rows
        rows.forEach(row => {
          let dataRow = '  │';
          fieldNames.forEach(field => {
            const width = colWidths[field];
            const value = String(row[field] || '');
            const displayValue = value.length > width ? value.slice(0, width - 3) + '...' : value;
            dataRow += ' ' + displayValue.padEnd(width) + ' │';
          });
          console.log(dataRow);
        });

        // Build bottom border
        let bottomLine = '  └';
        fieldNames.forEach(field => {
          const width = colWidths[field];
          bottomLine += '─'.repeat(width + 2) + '┴';
        });
        bottomLine = bottomLine.slice(0, -1) + '┘';
        console.log(bottomLine);

        // Show row count
        if (totalRows > rowCount) {
          console.log(`\n  Showing ${rowCount} of ${totalRows} rows`);
        } else {
          console.log(`\n  ${rowCount} row(s)`);
        }

        // Note about hidden columns only when --columns was explicitly specified
        const columnsDisplayed = fieldNames.length;
        let columnsHidden = [];

        if (columnsExplicit) {
          columnsHidden = obj.COLUMNS_HIDDEN || obj.columns_hidden || [];
          if (columnsHidden.length > 0) {
            console.log(`\n  ⚠️  ${columnsHidden.length} more columns hidden (${columnsHidden.join(', ')})`);
            console.log('     Use --json for full data');
          }
        }

        console.log('');
      }
    }
  }
};
