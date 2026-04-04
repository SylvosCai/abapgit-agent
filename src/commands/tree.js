/**
 * Tree command - Display package hierarchy tree
 */

/**
 * Run tree command and return raw result
 */
async function runTreeCommand(packageName, depth, includeTypes, csrfToken, http) {
  const data = {
    package: packageName,
    depth: depth,
    include_objects: includeTypes
  };

  return await http.post('/sap/bc/z_abapgit_agent/tree', data, { csrfToken });
}

/**
 * Build tree display lines from flat nodes list
 */
function buildTreeLinesFromNodes(nodes, prefix, isLast) {
  const lines = [];

  if (nodes.length === 0) {
    return lines;
  }

  // First node is the root
  const root = nodes[0];
  const icon = '📦';
  lines.push(`${prefix}${isLast ? '└─ ' : '├─ '} ${icon} ${root.PACKAGE || root.package}`);

  // Get children (nodes with depth > 0, grouped by depth)
  const children = nodes.filter(n => (n.DEPTH || n.depth) > 0);

  // Group children by depth
  const byDepth = {};
  children.forEach(n => {
    const d = n.DEPTH || n.depth;
    if (!byDepth[d]) byDepth[d] = [];
    byDepth[d].push(n);
  });

  // Process depth 1 children
  const depth1 = byDepth[1] || [];
  const newPrefix = prefix + (isLast ? '   ' : '│  ');

  depth1.forEach((child, idx) => {
    const childIsLast = idx === depth1.length - 1;
    const childLines = buildChildLines(child, newPrefix, childIsLast, byDepth);
    lines.push(...childLines);
  });

  return lines;
}

/**
 * Build lines for child nodes recursively
 */
function buildChildLines(node, prefix, isLast, byDepth) {
  const lines = [];
  const icon = '📦';
  const pkg = node.PACKAGE || node.package;

  lines.push(`${prefix}${isLast ? '└─ ' : '├─ '} ${icon} ${pkg}`);

  // Get children of this node
  const nodeDepth = node.DEPTH || node.depth;
  const children = (byDepth[nodeDepth + 1] || []).filter(n => {
    const parent = n.PARENT || n.parent;
    return parent === pkg;
  });

  const newPrefix = prefix + (isLast ? '   ' : '│  ');

  children.forEach((child, idx) => {
    const childIsLast = idx === children.length - 1;
    const childLines = buildChildLines(child, newPrefix, childIsLast, byDepth);
    lines.push(...childLines);
  });

  return lines;
}

/**
 * Display tree output
 */
async function displayTreeOutput(packageName, depth, includeTypes, loadConfig, AbapHttp) {
  const config = loadConfig();
  const http = new AbapHttp(config);

  const csrfToken = await http.fetchCsrfToken();

  console.log(`\n  Getting package tree for: ${packageName}`);

  const result = await runTreeCommand(packageName, depth, includeTypes, csrfToken, http);

  // Handle uppercase keys from ABAP
  const success = result.SUCCESS || result.success;
  const error = result.ERROR || result.error;

  if (!success || error) {
    console.error(`\n  ❌ Error: ${error || 'Failed to get tree'}`);
    return;
  }

  // Parse hierarchy structure (ABAP returns flat nodes with parent refs)
  const nodes = result.NODES || result.nodes || [];
  const rootPackage = result.PACKAGE || result.package || packageName;
  const parentPackage = result.PARENT_PACKAGE || result.parent_package;
  const totalPackages = result.TOTAL_PACKAGES || result.total_packages || 0;
  const totalObjects = result.TOTAL_OBJECTS || result.total_objects || 0;
  const objectTypes = result.OBJECTS || result.objects || [];

  console.log(`\n  Package Tree: ${rootPackage}`);

  // Display parent info if available
  if (parentPackage && parentPackage !== rootPackage) {
    console.log(`  ⬆️  Parent: ${parentPackage}`);
  }

  console.log('');

  // Build and display tree from flat nodes list
  const lines = buildTreeLinesFromNodes(nodes, '', true);
  for (const line of lines) {
    console.log(`  ${line}`);
  }

  console.log('');
  console.log('  Summary');
  console.log(`  PACKAGES: ${totalPackages}`);
  console.log(`  OBJECTS: ${totalObjects}`);

  // Display object types if available
  if (includeTypes && objectTypes.length > 0) {
    const typeStr = objectTypes.map(t => `${t.OBJECT || t.object}=${t.COUNT || t.count}`).join(' ');
    console.log(`  TYPES: ${typeStr}`);
  }
}

module.exports = {
  name: 'tree',
  description: 'Display package hierarchy tree from ABAP system',
  requiresAbapConfig: true,
  requiresVersionCheck: true,

  async execute(args, context) {
    const { loadConfig, AbapHttp } = context;

    if (args.includes('--help') || args.includes('-h')) {
      console.log(`
Usage:
  abapgit-agent tree --package <package> [--depth <n>] [--include-types] [--json]

Description:
  Display the package hierarchy tree from the ABAP system.

Parameters:
  --package <package>  Root package to display (required). Enclose $-packages in quotes.
  --depth <n>          Maximum depth to traverse (default: unlimited).
  --include-types      Show object type counts per package.
  --json               Output as JSON.

Examples:
  abapgit-agent tree --package ZMY_PACKAGE
  abapgit-agent tree --package '$MY_LOCAL_PKG'
  abapgit-agent tree --package ZMY_PACKAGE --depth 2 --include-types
`);
      return;
    }

    const packageArgIndex = args.indexOf('--package');
    if (packageArgIndex === -1) {
      console.error('Error: --package parameter required');
      console.error('Usage: abapgit-agent tree --package <package> [--depth <n>] [--include-types] [--json]');
      console.error('Example: abapgit-agent tree --package ZMY_PACKAGE');
      process.exit(1);
    }

    // Check if package value is missing (happens when shell variable expands to empty)
    if (packageArgIndex + 1 >= args.length) {
      console.error('Error: --package parameter value is missing');
      console.error('');
      console.error('Tip: If you are using a shell variable, make sure to quote it:');
      console.error('  abapgit-agent tree --package \'$ZMY_PACKAGE\'');
      console.error('  or escape the $ character:');
      console.error('  abapgit-agent tree --package \\$ZMY_PACKAGE');
      console.error('');
      console.error('Usage: abapgit-agent tree --package <package> [--depth <n>] [--include-types] [--json]');
      console.error('Example: abapgit-agent tree --package ZMY_PACKAGE');
      process.exit(1);
    }

    const packageName = args[packageArgIndex + 1];

    // Check for empty/whitespace-only package name
    if (!packageName || packageName.trim() === '') {
      console.error('Error: --package parameter cannot be empty');
      console.error('Usage: abapgit-agent tree --package <package> [--depth <n>] [--include-types] [--json]');
      console.error('Example: abapgit-agent tree --package ZMY_PACKAGE');
      process.exit(1);
    }

    // Optional depth parameter
    const depthArgIndex = args.indexOf('--depth');
    let depth = 3;
    if (depthArgIndex !== -1 && depthArgIndex + 1 < args.length) {
      depth = parseInt(args[depthArgIndex + 1], 10);
      if (isNaN(depth) || depth < 1) {
        console.error('Error: --depth must be a positive number');
        process.exit(1);
      }
    }

    // Optional include-types parameter (--include-objects is deprecated alias)
    const includeTypes = args.includes('--include-types') || args.includes('--include-objects');

    // Optional json parameter
    const jsonOutput = args.includes('--json');

    if (jsonOutput) {
      const config = loadConfig();
      const csrfToken = await fetchCsrfToken(config);
      const result = await runTreeCommand(packageName, depth, includeTypes, csrfToken, request);
      console.log(JSON.stringify(result, null, 2));
    } else {
      await displayTreeOutput(packageName, depth, includeTypes, loadConfig, AbapHttp);
    }
  }
};
