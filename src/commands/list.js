/**
 * List command - List ABAP objects in a package
 */

module.exports = {
  name: 'list',
  description: 'List ABAP objects in a package',
  requiresAbapConfig: true,
  requiresVersionCheck: true,

  async execute(args, context) {
    const { loadConfig, fetchCsrfToken, request } = context;

    const packageArgIndex = args.indexOf('--package');
    if (packageArgIndex === -1) {
      console.error('Error: --package parameter required');
      console.error('Usage: abapgit-agent list --package <package> [--type <types>] [--name <pattern>] [--limit <n>] [--offset <n>] [--json]');
      console.error('Example: abapgit-agent list --package \'$ZMY_PACKAGE\'');
      console.error('Example: abapgit-agent list --package \'$ZMY_PACKAGE\' --type CLAS,INTF');
      process.exit(1);
    }

    // Check if package value is missing
    if (packageArgIndex + 1 >= args.length) {
      console.error('Error: --package parameter value is missing');
      process.exit(1);
    }

    const packageName = args[packageArgIndex + 1];

    // Validate package name
    if (!packageName || packageName.trim() === '') {
      console.error('Error: --package parameter cannot be empty');
      process.exit(1);
    }

    // Optional type parameter
    const typeArgIndex = args.indexOf('--type');
    const type = typeArgIndex !== -1 && typeArgIndex + 1 < args.length ? args[typeArgIndex + 1] : null;

    // Optional name pattern
    const nameArgIndex = args.indexOf('--name');
    const name = nameArgIndex !== -1 && nameArgIndex + 1 < args.length ? args[nameArgIndex + 1] : null;

    // Optional limit
    const limitArgIndex = args.indexOf('--limit');
    let limit = 100;
    if (limitArgIndex !== -1 && limitArgIndex + 1 < args.length) {
      limit = parseInt(args[limitArgIndex + 1], 10);
      if (isNaN(limit) || limit < 1) {
        console.error('Error: --limit must be a positive number');
        process.exit(1);
      }
      if (limit > 1000) {
        console.error('Error: --limit value too high (max: 1000)');
        process.exit(1);
      }
    }

    // Optional offset
    const offsetArgIndex = args.indexOf('--offset');
    let offset = 0;
    if (offsetArgIndex !== -1 && offsetArgIndex + 1 < args.length) {
      offset = parseInt(args[offsetArgIndex + 1], 10);
      if (isNaN(offset) || offset < 0) {
        console.error('Error: --offset must be a non-negative number');
        process.exit(1);
      }
    }

    // Optional json parameter
    const jsonOutput = args.includes('--json');

    const config = loadConfig();
    const csrfToken = await fetchCsrfToken(config);

    const data = {
      package: packageName,
      limit: limit,
      offset: offset
    };

    if (type) {
      data.type = type;
    }

    if (name) {
      data.name = name;
    }

    const result = await request('POST', '/sap/bc/z_abapgit_agent/list', data, { csrfToken });

    // Handle uppercase keys from ABAP
    const success = result.SUCCESS || result.success;
    const error = result.ERROR || result.error;
    const objects = result.OBJECTS || result.objects || [];
    const byType = result.BY_TYPE || result.by_type || [];
    const total = result.TOTAL || result.total || 0;

    if (!success || error) {
      console.error(`\n  Error: ${error || 'Failed to list objects'}`);
      process.exit(1);
    }

    if (jsonOutput) {
      console.log(JSON.stringify(result, null, 2));
    } else {
      // Display human-readable output
      let title = `Objects in ${packageName}`;
      if (type) {
        title += ` (${type} only`;
        if (total !== objects.length) {
          title += `, Total: ${total}`;
        }
        title += ')';
      } else if (total !== objects.length) {
        title += ` (Total: ${total})`;
      }
      console.log(`\n${title}\n`);

      // Group objects by type
      const objectsByType = {};
      for (const obj of objects) {
        const objType = (obj.TYPE || obj.type || '???').toUpperCase();
        if (!objectsByType[objType]) {
          objectsByType[objType] = [];
        }
        objectsByType[objType].push(obj.NAME || obj.name);
      }

      // Display grouped objects
      for (const objType of Object.keys(objectsByType).sort()) {
        const objNames = objectsByType[objType];
        console.log(`  ${objType} (${objNames.length})`);
        for (const objName of objNames) {
          console.log(`    ${objName}`);
        }
        console.log('');
      }
    }
  }
};
