/**
 * Where command - Find where-used references for ABAP objects
 */

module.exports = {
  name: 'where',
  description: 'Find where-used references for ABAP objects',
  requiresAbapConfig: true,
  requiresVersionCheck: true,

  async execute(args, context) {
    const { loadConfig, fetchCsrfToken, request } = context;

    const objectsArgIndex = args.indexOf('--objects');
    if (objectsArgIndex === -1 || objectsArgIndex + 1 >= args.length) {
      console.error('Error: --objects parameter required');
      console.error('Usage: abapgit-agent where --objects <obj1>,<obj2>,... [--type <type>] [--limit <n>] [--offset <n>] [--json]');
      console.error('Example: abapgit-agent where --objects ZCL_MY_CLASS');
      console.error('Example: abapgit-agent where --objects ZIF_MY_INTERFACE');
      console.error('Example: abapgit-agent where --objects CL_SUT_AUNIT_RUNNER --limit 20');
      console.error('Example: abapgit-agent where --objects CL_SUT_AUNIT_RUNNER --offset 100');
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
    const jsonOutput = args.includes('--json');

    console.log(`\n  Where-used list for ${objects.length} object(s)`);

    const config = loadConfig();
    const csrfToken = await fetchCsrfToken(config);

    const data = {
      objects: objects,
      limit: Math.min(Math.max(1, limit), 500),
      offset: Math.max(0, offset)
    };

    if (type) {
      data.type = type;
    }

    const result = await request('POST', '/sap/bc/z_abapgit_agent/where', data, { csrfToken });

    // Handle uppercase keys from ABAP
    const success = result.SUCCESS || result.success;
    const whereObjects = result.OBJECTS || result.objects || [];
    const message = result.MESSAGE || result.message || '';
    const error = result.ERROR || result.error;
    const pagination = result.PAGINATION || result.pagination || null;

    if (!success || error) {
      console.error(`\n  Error: ${error || 'Failed to get where-used list'}`);
      return;
    }

    if (jsonOutput) {
      console.log(JSON.stringify(result, null, 2));
    } else {
      // Build pagination message
      let paginationMsg = '';
      const paginationTotal = pagination.TOTAL || pagination.total || 0;
      const paginationHasMore = pagination.HAS_MORE || pagination.has_more || false;
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

      for (let i = 0; i < whereObjects.length; i++) {
        const obj = whereObjects[i];
        const objName = obj.NAME || obj.name || `Object ${i + 1}`;
        const objType = obj.TYPE || obj.type || '';
        const error = obj.ERROR || obj.error || '';
        const references = obj.REFERENCES || obj.references || [];
        const count = obj.COUNT || obj.count || 0;

        // Handle object not found error
        if (error) {
          console.log(`  ❌ ${objName} (${objType})`);
          console.log(`     ${error}`);
          console.log('');
          continue;
        }

        if (count === 0) {
          console.log(`  ❌ ${objName} (${objType})`);
          console.log(`     No references found`);
          console.log('');
          continue;
        }

        console.log(`  🔍 ${objName} (${objType})`);
        console.log(`     Found ${count} reference(s):`);
        console.log('');

        // Display references - one line format: include → method (type) or include (type)
        for (let j = 0; j < references.length; j++) {
          const ref = references[j];
          const includeName = ref.INCLUDE_NAME || ref.include_name || '';
          const includeType = ref.INCLUDE_TYPE || ref.include_type || '';
          const methodName = ref.METHOD_NAME || ref.method_name || '';

          let line;
          if (methodName) {
            line = `  ${j + 1}. ${includeName} → ${methodName} (${includeType})`;
          } else {
            line = `  ${j + 1}. ${includeName} (${includeType})`;
          }
          console.log(line);
        }
        console.log('');
      }
    }
  }
};
