'use strict';

const fs = require('fs');
const path = require('path');
const { printHttpError } = require('../utils/format-error');
const { selectTransport, isNonInteractive } = require('../utils/transport-selector');

const SUPPORTED_SUFFIXES = [
  'attributeview',
  'calculationview'
];

const ENDPOINT = '/sap/bc/z_abapgit_agent/hana';

function deriveHanaPackage(filePath) {
  const normalised = filePath.replace(/\\/g, '/');
  const hanaIdx = normalised.indexOf('hana/');
  if (hanaIdx === -1) throw new Error(`File path does not contain a 'hana/' segment: ${filePath}`);
  const afterHana = normalised.slice(hanaIdx + 'hana/'.length);
  const parts = afterHana.split('/');
  parts.pop();
  if (parts.length === 0) throw new Error(`Cannot derive HANA package from file path: ${filePath}`);
  return parts.join('.');
}

function buildObjects(files) {
  const objects = [];
  for (const f of files) {
    const basename = path.basename(f);
    const dotIdx = basename.indexOf('.');
    if (dotIdx === -1) continue;
    const suffix = basename.slice(dotIdx + 1);
    if (!SUPPORTED_SUFFIXES.includes(suffix)) continue;
    const content = fs.readFileSync(f, 'utf8');
    objects.push({ filename: basename, type: suffix, content });
  }
  return objects;
}

function collectFiles(folderPath) {
  return fs.readdirSync(folderPath)
    .map(e => path.join(folderPath, e))
    .filter(e => fs.statSync(e).isFile())
    .filter(e => SUPPORTED_SUFFIXES.some(s => e.endsWith('.' + s)));
}

// Pad-and-clip helper matching the project-wide table style (transport, preview, customize).
function pad(s, w) {
  return String(s == null ? '' : s).substring(0, w).padEnd(w);
}

// Print a 2-space-gap header + ── divider line; returns the row-renderer.
// Usage: printTable([{key:'name', label:'Name', w:30}, ...], rows);
function printTable(cols, rows) {
  const header = '  ' + cols.map(c => pad(c.label, c.w)).join('  ');
  const divider = '  ' + cols.map(c => '─'.repeat(c.w)).join('  ');
  console.log(header);
  console.log(divider);
  for (const r of rows) {
    console.log('  ' + cols.map(c => pad(r[c.key], c.w)).join('  '));
    if (r._note) console.log(`    └─ ${r._note}`);
  }
}

function printDeployTable(results, dryRun) {
  const w1 = Math.max(35, ...results.map(r => (r.filename || '').length + 2));
  const cols = [
    { key: 'filename', label: 'File', w: w1 },
    { key: 'type',     label: 'Type', w: 20 },
    { key: 'status',   label: dryRun ? 'Valid' : 'Status', w: dryRun ? 5 : 12 }
  ];
  const rows = results.map(r => ({
    filename: r.filename || '',
    type:     r.type || '',
    status:   dryRun ? (r.valid !== false ? 'yes' : 'no') : (r.status || '-'),
    _note:    r.message
  }));
  printTable(cols, rows);
}

async function runDeploy(args, context) {
  const { loadConfig, AbapHttp } = context;

  if (args.includes('--help') || args.includes('-h')) {
    console.log(`
Usage:
  abapgit-agent hana view deploy --file <path>   [options]
  abapgit-agent hana view deploy --folder <path> [options]

Options:
  --file <path>           Path to a single HANA artifact file
  --folder <path>         Path to a package folder; deploys all supported files
  --transport <trkorr>    Existing transport request
  --abap-package <devc>   ABAP devclass for first-time deploy (not yet in TADIR)
  --force                 Re-deploy even if object is already active in HANA
  --dry-run               Validate without writing to HOT tables, HANA, or transport

Supported file types:
  .attributeview  .calculationview

Examples:
  abapgit-agent hana view deploy --file hana/com/example/myapp/SalesAnalytics.calculationview
  abapgit-agent hana view deploy --folder hana/com/example/myapp/
  abapgit-agent hana view deploy --folder hana/com/example/myapp/ --transport DEVK900123
  abapgit-agent hana view deploy --folder hana/com/example/myapp/ --abap-package ZMYAPP
  abapgit-agent hana view deploy --folder hana/com/example/myapp/ --dry-run
`);
    return;
  }

  const dryRun = args.includes('--dry-run');
  const force  = args.includes('--force');

  const fileIdx   = args.indexOf('--file');
  const folderIdx = args.indexOf('--folder');
  const trIdx     = args.indexOf('--transport');
  const pkgIdx    = args.indexOf('--abap-package');

  if (fileIdx === -1 && folderIdx === -1) {
    console.error('Error: --file or --folder is required');
    process.exit(1);
  }
  if (fileIdx !== -1 && folderIdx !== -1) {
    console.error('Error: --file and --folder are mutually exclusive');
    process.exit(1);
  }

  const transport   = trIdx  !== -1 ? args[trIdx  + 1] : undefined;
  const abapPackage = pkgIdx !== -1 ? args[pkgIdx + 1] : undefined;

  let files = [];
  let hanaPackage;

  if (fileIdx !== -1) {
    const filePath = args[fileIdx + 1];
    if (!fs.existsSync(filePath)) {
      console.error(`Error: File not found: ${filePath}`);
      process.exit(1);
    }
    files = [filePath];
    hanaPackage = deriveHanaPackage(filePath);
  } else {
    const folderPath = args[folderIdx + 1];
    if (!fs.existsSync(folderPath)) {
      console.error(`Error: Folder not found: ${folderPath}`);
      process.exit(1);
    }
    files = collectFiles(folderPath);
    hanaPackage = deriveHanaPackage(path.join(folderPath, '_placeholder'));
    if (files.length === 0) {
      console.log('No supported HANA files found.');
      return;
    }
  }

  const objects = buildObjects(files);
  if (objects.length === 0) {
    console.log('No supported HANA files found.');
    return;
  }

  const config = loadConfig();
  const http   = new AbapHttp(config);

  // Fall back to repo package from .abapGitAgent when --abap-package not supplied
  const resolvedAbapPackage = abapPackage || config.package || '';

  // Phase 1: check transportability (skip for dry-run — no transport needed)
  let resolvedTransport = transport || '';
  if (!dryRun && !resolvedTransport) {
    let checkResult;
    try {
      const raw = await http.post(ENDPOINT, { action: 'check', hanaPackage, abapPackage: resolvedAbapPackage });
      checkResult = typeof raw === 'string' ? JSON.parse(raw) : raw;
    } catch (err) {
      printHttpError(err);
      process.exit(1);
    }

    if (checkResult.error) {
      console.error(`❌ Error: ${checkResult.error}`);
      if (checkResult.unregistered_packages) {
        console.error(`\n  HANA package not registered in TADIR: ${checkResult.unregistered_packages}`);
        if (!resolvedAbapPackage) {
          console.error('  Re-run with --abap-package <devclass>.');
        }
      }
      process.exit(1);
    }

    if (checkResult.transport_required === true || checkResult.transport_required === 'true') {
      resolvedTransport = await selectTransport(config, http, loadConfig, AbapHttp);
      if (!resolvedTransport && !isNonInteractive()) {
        console.error('Error: A transport request is required for this package.');
        process.exit(1);
      }
    }
  }

  // Phase 2: deploy
  const body = {
    action:      'deploy',
    hanaPackage,
    abapPackage: resolvedAbapPackage,
    transport:   resolvedTransport,
    force,
    dryRun,
    objects
  };

  console.log(`\n🚀 ${dryRun ? 'Dry-run deploy' : 'Deploying'} to HANA  (package: ${hanaPackage})\n`);

  let response;
  try {
    const raw = await http.post(ENDPOINT, body);
    response = typeof raw === 'string' ? JSON.parse(raw) : raw;
  } catch (err) {
    printHttpError(err);
    process.exit(1);
  }

  if (response.error) {
    console.error(`❌ Error: ${response.error}`);
    if (response.unregistered_packages) {
      console.error(`\n  HANA package not registered in TADIR: ${response.unregistered_packages}`);
      console.error('  Re-run with --abap-package <devclass>.');
    }
    if (response.results) printDeployTable(response.results, dryRun);
    process.exit(1);
  }

  printDeployTable(response.results || [], dryRun);

  if (dryRun) {
    console.log(`\n  ${response.deployed || objects.length} object(s) would be deployed.`);
  } else {
    const tr      = response.transport ? `  Transport: ${response.transport}` : '';
    const skipped = response.skipped > 0 ? `  Skipped: ${response.skipped} (already active)` : '';
    console.log(`\n✅ Deployed ${response.deployed} object(s).${tr}${skipped}`);
    if (response.skipped > 0) console.log('   Use --force to redeploy already-active objects.');
  }
}

async function runList(args, context) {
  const { loadConfig, AbapHttp } = context;

  if (args.includes('--help') || args.includes('-h')) {
    console.log(`
Usage:
  abapgit-agent hana view list --package <id> [options]

Options:
  --package <id>    HANA package ID to list (e.g. com.example.myapp)
  --subpackages     Include sub-packages recursively
  --json            Output raw JSON

Examples:
  abapgit-agent hana view list --package com.example.myapp
  abapgit-agent hana view list --package com.example --subpackages
  abapgit-agent hana view list --package com.example.myapp --json
`);
    return;
  }

  const pkgIdx = args.indexOf('--package');
  if (pkgIdx === -1) {
    console.error('Error: --package is required');
    process.exit(1);
  }

  const hanaPackage  = args[pkgIdx + 1];
  const subpackages  = args.includes('--subpackages');
  const jsonOutput   = args.includes('--json');

  const config = loadConfig();
  const http   = new AbapHttp(config);

  let response;
  try {
    const raw = await http.post(ENDPOINT, {
      action:      'list',
      hanaPackage,
      subpackages
    });
    response = typeof raw === 'string' ? JSON.parse(raw) : raw;
  } catch (err) {
    const { printHttpError } = require('../utils/format-error');
    printHttpError(err);
    process.exit(1);
  }

  if (response.error) {
    console.error(`❌ Error: ${response.error}`);
    process.exit(1);
  }

  if (jsonOutput) {
    console.log(JSON.stringify(response, null, 2));
    return;
  }

  const objects = response.objects || [];
  console.log(`\n📋 HANA package: ${hanaPackage}\n`);

  if (objects.length === 0) {
    console.log('  No objects found in HTA repository for this package.');
    console.log(`  If the package exists in HANA but not in HTA, run: abapgit-agent hana view import --package ${hanaPackage}`);
    return;
  }

  const w1 = Math.max(26, ...objects.map(o => (o.name   || '').length + 2));
  const w2 = Math.max(18, ...objects.map(o => (o.suffix || '').length + 2));
  printTable(
    [
      { key: 'name',         label: 'Object',       w: w1 },
      { key: 'suffix',       label: 'Suffix',       w: w2 },
      { key: 'deploy_state', label: 'Deploy state', w: 13 },
      { key: 'hot_status',   label: 'HOT status',   w: 10 }
    ],
    objects.map(o => ({
      name:         o.name,
      suffix:       o.suffix,
      deploy_state: o.deploy_state,
      hot_status:   o.hot_status || '-'
    }))
  );

  const notDeployed = objects.filter(o => o.deploy_state !== 'deployed').length;
  const folderPath  = 'hana/' + hanaPackage.replace(/\./g, '/') + '/';
  if (notDeployed > 0) {
    console.log(`\n  ${objects.length} object(s) found.  ⚠️  ${notDeployed} not deployed — run: abapgit-agent hana view deploy --folder ${folderPath}`);
  } else {
    console.log(`\n  ${objects.length} object(s) found.`);
  }
}

async function runImport(args, context) {
  const { loadConfig, AbapHttp } = context;

  if (args.includes('--help') || args.includes('-h')) {
    console.log(`
Usage:
  abapgit-agent hana view import --package <id> [options]

Options:
  --package <id>         HANA package ID to import (e.g. com.example.myapp)
  --abap-package <devc>  ABAP devclass for HTA registration (if not yet in TADIR)
  --output <path>        Output folder root (default: hana/)
  --dry-run              Show what would be written, without creating files

Examples:
  abapgit-agent hana view import --package com.example.myapp
  abapgit-agent hana view import --package com.example.myapp --abap-package ZMYAPP
  abapgit-agent hana view import --package com.example.myapp --dry-run
`);
    return;
  }

  const pkgIdx    = args.indexOf('--package');
  const abapIdx   = args.indexOf('--abap-package');
  const outputIdx = args.indexOf('--output');
  const dryRun    = args.includes('--dry-run');

  if (pkgIdx === -1) {
    console.error('Error: --package is required');
    process.exit(1);
  }

  const hanaPackage  = args[pkgIdx + 1];
  const abapPackage  = abapIdx  !== -1 ? args[abapIdx  + 1] : undefined;
  const outputRoot   = outputIdx !== -1 ? args[outputIdx + 1] : 'hana';

  const config = loadConfig();
  const http   = new AbapHttp(config);
  const resolvedAbapPackage = abapPackage || config.package || '';

  console.log(`\n🚀 Importing from HANA  (package: ${hanaPackage})\n`);

  let response;
  try {
    const raw = await http.post(ENDPOINT, {
      action:      'import',
      hanaPackage,
      abapPackage: resolvedAbapPackage
    });
    response = typeof raw === 'string' ? JSON.parse(raw) : raw;
  } catch (err) {
    printHttpError(err);
    process.exit(1);
  }

  if (!response.success) {
    console.error(`❌ Error: ${response.error}`);
    process.exit(1);
  }

  const objects = response.objects || [];
  if (objects.length === 0) {
    console.log('  No objects found in HANA package.');
    return;
  }

  const folderPath = path.join(outputRoot, ...hanaPackage.split('.'));
  const w1 = Math.max(30, ...objects.map(o => (o.name || '').length + 2));
  const w2 = Math.max(18, ...objects.map(o => (o.suffix || '').length + 2));

  // Write files first so the printed status reflects reality
  let written = 0;
  let errors  = 0;
  const rows = [];
  for (const o of objects) {
    let status;
    if (o.status === 'ok') {
      if (dryRun) {
        status = 'would write';
      } else {
        const filePath = path.join(folderPath, `${o.name}.${o.suffix}`);
        fs.mkdirSync(folderPath, { recursive: true });
        fs.writeFileSync(filePath, o.content || '', 'utf8');
        status = 'written';
        written++;
      }
    } else {
      status = `error: ${o.message}`;
      errors++;
    }
    rows.push({ name: o.name, suffix: o.suffix, status });
  }

  printTable(
    [
      { key: 'name',   label: 'Object', w: w1 },
      { key: 'suffix', label: 'Suffix', w: w2 },
      { key: 'status', label: 'Status', w: 24 }
    ],
    rows
  );

  if (dryRun) {
    console.log(`\n  ${objects.length} object(s) would be written to ${folderPath}/`);
  } else {
    console.log(`\n✅ Wrote ${written} object(s) to ${folderPath}/`);
  }
  if (errors > 0) {
    console.error(`❌ ${errors} object(s) failed to import.`);
    process.exit(1);
  }
}


async function runDrop(args, context) {
  const { loadConfig, AbapHttp } = context;

  if (args.includes('--help') || args.includes('-h')) {
    console.log(`
Usage:
  abapgit-agent hana view drop --file <path> [--dry-run]

Description:
  Delete a single HANA view from the HANA repository. The local file under
  hana/ is kept — accidental drops can be recovered with:
    abapgit-agent hana view deploy --file <path> --force

Options:
  --file <path>   Local path; encodes HANA package + name + suffix
  --dry-run       Show what would be dropped without changes

Examples:
  abapgit-agent hana view drop --file hana/com/example/myapp/SalesAnalytics.calculationview
  abapgit-agent hana view drop --file hana/com/example/myapp/CustomerSegments.attributeview --dry-run
`);
    return;
  }

  const dryRun  = args.includes('--dry-run');
  const fileIdx = args.indexOf('--file');
  if (fileIdx === -1) {
    console.error('Error: --file is required');
    process.exit(1);
  }
  const filePath = args[fileIdx + 1];
  if (!filePath) {
    console.error('Error: --file requires a path argument');
    process.exit(1);
  }
  if (!fs.existsSync(filePath)) {
    console.error(`Error: File not found: ${filePath}`);
    console.error('The local file is required as the drop contract.');
    process.exit(1);
  }

  const basename = path.basename(filePath);
  const dotIdx = basename.indexOf('.');
  if (dotIdx === -1) {
    console.error(`Error: cannot parse object name/suffix from filename: ${basename}`);
    process.exit(1);
  }
  const objectName   = basename.slice(0, dotIdx);
  const objectSuffix = basename.slice(dotIdx + 1);
  if (!SUPPORTED_SUFFIXES.includes(objectSuffix)) {
    console.error(`Error: Unsupported file type '.${objectSuffix}'. Supported: ${SUPPORTED_SUFFIXES.map(s => '.' + s).join(', ')}`);
    process.exit(1);
  }

  const hanaPackage = deriveHanaPackage(filePath);
  const config = loadConfig();
  const http   = new AbapHttp(config);

  console.log(`\n🚀 ${dryRun ? 'Dry-run drop' : 'Dropping'} from HANA  (package: ${hanaPackage})\n`);

  let response;
  try {
    const raw = await http.post(ENDPOINT, {
      action:       'drop',
      hanaPackage,
      objectName,
      objectSuffix,
      dryRun
    });
    response = typeof raw === 'string' ? JSON.parse(raw) : raw;
  } catch (err) {
    printHttpError(err);
    process.exit(1);
  }

  const results = response.results || [];
  if (results.length > 0) {
    const w1 = Math.max(30, ...results.map(r => (r.filename || '').length + 2));
    const cols = [
      { key: 'filename', label: 'Object', w: w1 },
      { key: 'status',   label: 'Status', w: 14 }
    ];
    const rows = results.map(r => ({
      filename: r.filename || '',
      status:   r.status === 'would_drop' ? 'would drop' : (r.status || '-'),
      _note:    r.message
    }));
    printTable(cols, rows);
  }

  if (response.error) {
    console.error(`\n❌ Error: ${response.error}`);
    process.exit(1);
  }

  if (dryRun) {
    console.log(`\n  ${response.deployed || 0} object(s) would be dropped.`);
  } else {
    const item = results[0] || {};
    if (item.status === 'skipped') {
      console.log(`\n✅ No objects dropped (already absent in HANA).`);
    } else {
      console.log(`\n✅ Dropped 1 object from HANA.`);
      console.log(`   Local file kept: ${filePath}`);
    }
  }
}


async function runPreview(args, context) {
  const { loadConfig, AbapHttp } = context;

  if (args.includes('--help') || args.includes('-h')) {
    console.log(`
Usage:
  abapgit-agent hana view preview --file <path> [options]

Description:
  Preview the data produced by a HANA view (activated under _SYS_BIC).

Options:
  --file <path>      Local path; encodes HANA package + view name
  --limit <n>        Max rows to return (default 100, max 500)
  --offset <n>       Skip first N rows for paging (default 0)
  --where "<expr>"   WHERE clause body (no WHERE keyword)
  --columns A,B,C    Project only the given columns
  --vertical         Render one column per line (good for wide views)
  --compact          Truncate cell values to column width
  --json             Output raw JSON

Examples:
  abapgit-agent hana view preview --file hana/com/example/myapp/SalesAnalytics.calculationview
  abapgit-agent hana view preview --file hana/.../SalesAnalytics.calculationview --limit 20
  abapgit-agent hana view preview --file hana/.../SalesAnalytics.calculationview --where "MESSAGE = 'integration test'"
  abapgit-agent hana view preview --file hana/.../SalesAnalytics.calculationview --columns MESSAGE,DUMMY_MEASURE
  abapgit-agent hana view preview --file hana/.../SalesAnalytics.calculationview --vertical --limit 5
`);
    return;
  }

  const fileIdx = args.indexOf('--file');
  if (fileIdx === -1) {
    console.error('Error: --file is required');
    process.exit(1);
  }
  const filePath = args[fileIdx + 1];
  if (!filePath) {
    console.error('Error: --file requires a path argument');
    process.exit(1);
  }
  if (!fs.existsSync(filePath)) {
    console.error(`Error: File not found: ${filePath}`);
    console.error('The local file is required as the preview contract.');
    process.exit(1);
  }

  const basename = path.basename(filePath);
  const dotIdx = basename.indexOf('.');
  if (dotIdx === -1) {
    console.error(`Error: cannot parse object name/suffix from filename: ${basename}`);
    process.exit(1);
  }
  const objectName   = basename.slice(0, dotIdx);
  const objectSuffix = basename.slice(dotIdx + 1);
  if (!SUPPORTED_SUFFIXES.includes(objectSuffix)) {
    console.error(`Error: Unsupported file type '.${objectSuffix}'. Supported: ${SUPPORTED_SUFFIXES.map(s => '.' + s).join(', ')}`);
    process.exit(1);
  }

  const hanaPackage = deriveHanaPackage(filePath);

  // Optional flags
  const limitIdx   = args.indexOf('--limit');
  const offsetIdx  = args.indexOf('--offset');
  const whereIdx   = args.indexOf('--where');
  const columnsIdx = args.indexOf('--columns');
  const vertical   = args.includes('--vertical');
  const compact    = args.includes('--compact');
  const jsonOutput = args.includes('--json');

  const limit   = limitIdx  !== -1 ? parseInt(args[limitIdx  + 1], 10) : 0;
  const offset  = offsetIdx !== -1 ? parseInt(args[offsetIdx + 1], 10) : 0;
  const where   = whereIdx  !== -1 ? args[whereIdx + 1] : '';
  const columns = columnsIdx !== -1 ? args[columnsIdx + 1].split(',').map(s => s.trim()).filter(Boolean) : [];

  const config = loadConfig();
  const http   = new AbapHttp(config);

  let response;
  try {
    const raw = await http.post(ENDPOINT, {
      action:       'preview',
      hanaPackage,
      objectName,
      objectSuffix,
      limit,
      offset,
      where,
      columns
    });
    response = typeof raw === 'string' ? JSON.parse(raw) : raw;
  } catch (err) {
    printHttpError(err);
    process.exit(1);
  }

  if (jsonOutput) {
    console.log(JSON.stringify(response, null, 2));
    if (response.success === false) process.exit(1);
    return;
  }

  if (response.success === false) {
    console.error(`\n❌ Error: ${response.error || 'preview_failed'}`);
    if (response.message) {
      const lines = String(response.message).split('\n');
      for (const line of lines) console.error(`   ${line}`);
    }
    process.exit(1);
  }

  const cols = response.columns || [];
  const rows = response.rows || [];
  const viewKey = response.view || `${hanaPackage}/${objectName}`;
  const ctxParts = [];
  if (offset > 0)    ctxParts.push(`offset=${offset}`);
  if (response.limit) ctxParts.push(`limit=${response.limit}`);
  if (where)         ctxParts.push(`where: ${where}`);
  const ctxSuffix = ctxParts.length ? `  (${ctxParts.join(', ')})` : '';

  console.log(`\n📋 HANA view: ${viewKey}${ctxSuffix}\n`);

  if (rows.length === 0) {
    console.log('  No rows returned.');
    return;
  }

  if (vertical) {
    const colNames = cols.map(c => c.name);
    const nameWidth = Math.max(...colNames.map(n => n.length));
    rows.forEach((row, i) => {
      const cells = row.cells || row;  // tolerate both shapes
      console.log(`────── Row ${i + 1} ──────`);
      colNames.forEach((name, j) => {
        const v = cells[j] == null ? '<null>' : String(cells[j]);
        console.log(`  ${name.padEnd(nameWidth)} : ${v}`);
      });
      console.log('');
    });
    console.log(`  ${rows.length} row(s) returned.`);
    return;
  }

  // Horizontal table: compute widths from data + header
  const widths = cols.map((c, i) => {
    const header = String(c.name || '').length;
    const dataMax = rows.reduce((m, r) => {
      const cells = r.cells || r;
      const v = cells[i] == null ? '<null>' : String(cells[i]);
      return Math.max(m, v.length);
    }, 0);
    const w = Math.max(header, dataMax) + 2;
    return compact ? Math.min(w, 30) : w;
  });

  const renderCell = (s, w) => {
    const v = s == null ? '<null>' : String(s);
    return compact ? v.substring(0, w).padEnd(w) : v.padEnd(w);
  };

  const header = '  ' + cols.map((c, i) => renderCell(c.name, widths[i])).join('  ');
  const divider = '  ' + widths.map(w => '─'.repeat(w)).join('  ');
  console.log(header);
  console.log(divider);
  for (const r of rows) {
    const cells = r.cells || r;
    console.log('  ' + cells.map((v, i) => renderCell(v, widths[i])).join('  '));
  }
  console.log(`\n  ${rows.length} row(s) returned.${offset > 0 || rows.length === (response.limit || 100) ? `  (Use --offset ${offset + rows.length} for the next page.)` : ''}`);
}


async function runSchema(args, context) {
  const { loadConfig, AbapHttp } = context;

  const subaction = args[0];
  const subArgs = args.slice(1);

  if (!subaction || subArgs.includes('--help') || subArgs.includes('-h') || subaction === '--help' || subaction === '-h') {
    console.log(`
Usage:
  abapgit-agent hana schema list   [--filter <pattern>] [--limit <n>] [--json]
  abapgit-agent hana schema tables --schema <name> [--filter <pattern>] [--limit <n>] [--json]
  abapgit-agent hana schema fields --schema <name> --table <name> [--json]

Subcommands:
  list     List HANA schemas
  tables   List tables in a HANA schema
  fields   List columns of a HANA table

Options:
  --schema <name>      HANA schema name (required for tables/fields)
  --table <name>       HANA table name (required for fields)
  --filter <pattern>   SQL LIKE pattern, e.g. %FRA% or T0%
  --limit <n>          Max rows to return (default 100; not used for fields)
  --json               Output raw JSON

Examples:
  abapgit-agent hana schema list --filter %CORE%
  abapgit-agent hana schema tables --schema SAP_FRA_CORE_S4 --filter T001
  abapgit-agent hana schema fields --schema SAP_FRA_CORE_S4 --table T001
`);
    return;
  }

  if (!['list', 'tables', 'fields'].includes(subaction)) {
    console.error(`Error: Unknown schema subcommand '${subaction}'. Use: list, tables, fields`);
    process.exit(1);
  }

  const schemaIdx = subArgs.indexOf('--schema');
  const tableIdx  = subArgs.indexOf('--table');
  const filterIdx = subArgs.indexOf('--filter');
  const limitIdx  = subArgs.indexOf('--limit');

  const schemaArg = schemaIdx !== -1 ? subArgs[schemaIdx + 1] : '';
  const tableArg  = tableIdx  !== -1 ? subArgs[tableIdx  + 1] : '';
  const filter    = filterIdx !== -1 ? subArgs[filterIdx + 1] : '';
  const limit     = limitIdx  !== -1 ? parseInt(subArgs[limitIdx + 1], 10) : 0;
  const jsonOutput = subArgs.includes('--json');

  if (subaction === 'tables' && !schemaArg) {
    console.error('Error: --schema is required for "tables"');
    process.exit(1);
  }
  if (subaction === 'fields' && (!schemaArg || !tableArg)) {
    console.error('Error: --schema and --table are required for "fields"');
    process.exit(1);
  }

  const config = loadConfig();
  const http   = new AbapHttp(config);

  let response;
  try {
    const raw = await http.post(ENDPOINT, {
      action:    'schema',
      subaction,
      schema:    schemaArg,
      table:     tableArg,
      filter,
      limit
    });
    response = typeof raw === 'string' ? JSON.parse(raw) : raw;
  } catch (err) {
    printHttpError(err);
    process.exit(1);
  }

  if (response.error || response.success === false) {
    console.error(`❌ Error: ${response.error || 'unknown error'}`);
    process.exit(1);
  }

  if (jsonOutput) {
    console.log(JSON.stringify(response, null, 2));
    return;
  }

  const rows = response.rows || [];
  if (rows.length === 0) {
    console.log('\n  No matches found.');
    return;
  }

  if (subaction === 'list') {
    const w1 = Math.max(20, ...rows.map(r => (r.schema_name || '').length + 2));
    console.log('');
    printTable([{ key: 'schema_name', label: 'Schema', w: w1 }], rows);
    console.log(`\n  ${rows.length} schema(s) found.`);
    return;
  }

  if (subaction === 'tables') {
    const w1 = Math.max(30, ...rows.map(r => (r.table_name || '').length + 2));
    console.log(`\n📋 Schema: ${schemaArg.toUpperCase()}\n`);
    printTable(
      [
        { key: 'table_name',      label: 'Table',   w: w1 },
        { key: 'is_column_table', label: 'Column?', w: 12 }
      ],
      rows
    );
    console.log(`\n  ${rows.length} table(s) found.`);
    return;
  }

  if (subaction === 'fields') {
    const w2 = Math.max(20, ...rows.map(r => (r.column_name || '').length + 2));
    const w3 = Math.max(18, ...rows.map(r => (r.data_type_name || '').length + 2));
    console.log(`\n📋 Schema: ${schemaArg.toUpperCase()}   Table: ${tableArg.toUpperCase()}\n`);
    printTable(
      [
        { key: 'position',       label: 'Pos',      w: 5 },
        { key: 'column_name',    label: 'Column',   w: w2 },
        { key: 'data_type_name', label: 'Type',     w: w3 },
        { key: 'length',         label: 'Length',   w: 8 },
        { key: 'scale',          label: 'Scale',    w: 8 },
        { key: 'is_nullable',    label: 'Nullable', w: 10 }
      ],
      rows
    );
    console.log(`\n  ${rows.length} column(s) found.`);
  }
}


module.exports = {
  name: 'hana',
  description: 'Manage HANA content (view, schema)',
  requiresAbapConfig: true,
  requiresVersionCheck: false,

  async execute(args, context) {
    const sub = args[0];
    const subArgs = args.slice(1);

    const knownSubs = ['view', 'schema', 'table'];
    if (!sub || (!knownSubs.includes(sub) && (args.includes('--help') || args.includes('-h')))) {
      console.log(`
Usage:
  abapgit-agent hana <subcommand> [options]

Subcommands:
  view     Manage HANA repository views (list, deploy, import, drop, preview)
  schema   Query HANA catalog (list schemas, list tables, list columns)
  table    Preview raw HANA table data
Run 'abapgit-agent hana <subcommand> --help' for subcommand options.
`);
      return;
    }

    switch (sub) {
      case 'view':
        return runView(subArgs, context);
      case 'schema':
        return runSchema(subArgs, context);
      case 'table':
        return runTable(subArgs, context);
      default:
        console.error(`Error: Unknown subcommand '${sub}'. Use: view, schema, table`);
        process.exit(1);
    }
  }
};

async function runView(args, context) {
  const subaction = args[0];
  const subArgs = args.slice(1);

  if (!subaction || subaction === '--help' || subaction === '-h') {
    console.log(`
Usage:
  abapgit-agent hana view <subcommand> [options]

Subcommands:
  deploy   Push local hana/ files into HANA repository
  import   Bootstrap existing HANA content into local hana/ folder (one-time)
  list     List objects in a HANA package and their deployment status
  drop     Delete a HANA view from the HANA repository (local file is kept)
  preview  Preview the data produced by an activated HANA view (via _SYS_BIC)
Run 'abapgit-agent hana view <subcommand> --help' for subcommand options.
`);
    return;
  }

  switch (subaction) {
    case 'deploy':
      return runDeploy(subArgs, context);
    case 'import':
      return runImport(subArgs, context);
    case 'list':
      return runList(subArgs, context);
    case 'drop':
      return runDrop(subArgs, context);
    case 'preview':
      return runPreview(subArgs, context);
    default:
      console.error(`Error: Unknown view subcommand '${subaction}'. Use: deploy, import, list, drop, preview`);
      process.exit(1);
  }
}


async function runTable(args, context) {
  const subaction = args[0];
  const subArgs = args.slice(1);

  if (!subaction || subaction === '--help' || subaction === '-h') {
    console.log(`
Usage:
  abapgit-agent hana table <subcommand> [options]

Subcommands:
  preview  Preview rows from a raw HANA table
Run 'abapgit-agent hana table <subcommand> --help' for subcommand options.
`);
    return;
  }

  switch (subaction) {
    case 'preview':
      return runTablePreview(subArgs, context);
    default:
      console.error(`Error: Unknown table subcommand '${subaction}'. Use: preview`);
      process.exit(1);
  }
}


async function runTablePreview(args, context) {
  const { loadConfig, AbapHttp } = context;

  if (args.includes('--help') || args.includes('-h')) {
    console.log(`
Usage:
  abapgit-agent hana table preview --schema SCHEMA --table TABLE [options]
  abapgit-agent hana table preview --logical-schema NAME --table TABLE [options]

Description:
  Preview rows from a raw HANA table. Targets either a physical schema name
  (e.g. SAP_FRA_CORE_S4) or a logical schema (resolved via DB_SCHEMA_MAP).

Options:
  --schema <name>          Physical HANA schema name (mutually exclusive with --logical-schema)
  --logical-schema <name>  Logical schema name (resolved via DB_SCHEMA_MAP / $ABAP.schema)
  --table <name>           HANA table name (required)
  --limit <n>              Max rows to return (default 100, max 500)
  --offset <n>             Skip first N rows for paging (default 0)
  --where "<expr>"         WHERE clause body (no WHERE keyword)
  --columns A,B,C          Project only the given columns
  --vertical               Render one column per line
  --compact                Truncate cell values to column width
  --json                   Output raw JSON

Examples:
  abapgit-agent hana table preview --schema SAP_FRA_CORE_S4 --table T001
  abapgit-agent hana table preview --schema SAP_FRA_CORE_S4 --table T001 --columns BUKRS,BUTXT
  abapgit-agent hana table preview --schema SAP_FRA_CORE_S4 --table T001 --where "BUKRS = '1000'"
  abapgit-agent hana table preview --logical-schema FRA_CORE_ERP --table T001
  abapgit-agent hana table preview --schema SYS --table DUMMY --json
`);
    return;
  }

  const schemaIdx        = args.indexOf('--schema');
  const logicalSchemaIdx = args.indexOf('--logical-schema');
  const tableIdx         = args.indexOf('--table');
  const limitIdx         = args.indexOf('--limit');
  const offsetIdx        = args.indexOf('--offset');
  const whereIdx         = args.indexOf('--where');
  const columnsIdx       = args.indexOf('--columns');
  const vertical         = args.includes('--vertical');
  const compact          = args.includes('--compact');
  const jsonOutput       = args.includes('--json');

  const schema        = schemaIdx        !== -1 ? args[schemaIdx        + 1] : '';
  const logicalSchema = logicalSchemaIdx !== -1 ? args[logicalSchemaIdx + 1] : '';
  const table         = tableIdx         !== -1 ? args[tableIdx         + 1] : '';

  if (!table) {
    console.error('Error: --table is required');
    process.exit(1);
  }
  if (schema && logicalSchema) {
    console.error('Error: --schema and --logical-schema are mutually exclusive — provide exactly one');
    process.exit(1);
  }
  if (!schema && !logicalSchema) {
    console.error('Error: provide exactly one of --schema <physical> or --logical-schema <logical>');
    process.exit(1);
  }

  const limit   = limitIdx   !== -1 ? parseInt(args[limitIdx  + 1], 10) : 0;
  const offset  = offsetIdx  !== -1 ? parseInt(args[offsetIdx + 1], 10) : 0;
  const where   = whereIdx   !== -1 ? args[whereIdx + 1] : '';
  const columns = columnsIdx !== -1 ? args[columnsIdx + 1].split(',').map(s => s.trim()).filter(Boolean) : [];

  const config = loadConfig();
  const http   = new AbapHttp(config);

  let response;
  try {
    const raw = await http.post(ENDPOINT, {
      action:        'tablePreview',
      schema,
      logicalSchema,
      table,
      limit,
      offset,
      where,
      columns
    });
    response = typeof raw === 'string' ? JSON.parse(raw) : raw;
  } catch (err) {
    printHttpError(err);
    process.exit(1);
  }

  if (jsonOutput) {
    console.log(JSON.stringify(response, null, 2));
    if (response.success === false) process.exit(1);
    return;
  }

  if (response.success === false) {
    console.error(`\n❌ Error: ${response.error || 'preview_failed'}`);
    if (response.message) {
      const lines = String(response.message).split('\n');
      for (const line of lines) console.error(`   ${line}`);
    }
    if (response.error === 'table_not_found' && schema) {
      console.error(`   Hint: list tables with \`abapgit-agent hana schema tables --schema ${schema.toUpperCase()} --filter ${table.toUpperCase()}%\``);
    } else if (response.error === 'schema_not_mapped') {
      console.error('   Configure the logical schema in transaction DB_SCHEMA_MAP and retry.');
    }
    process.exit(1);
  }

  const cols = response.columns || [];
  const rows = response.rows || [];
  const tableKey = response.table || `${schema || logicalSchema}.${table}`;
  const ctxParts = [];
  if (offset > 0)         ctxParts.push(`offset=${offset}`);
  if (response.limit)     ctxParts.push(`limit=${response.limit}`);
  if (where)              ctxParts.push(`where: ${where}`);
  const ctxSuffix = ctxParts.length ? `  (${ctxParts.join(', ')})` : '';

  console.log(`\n📋 HANA table: ${tableKey}${ctxSuffix}\n`);

  if (rows.length === 0) {
    console.log('  No rows returned.');
    return;
  }

  if (vertical) {
    const colNames = cols.map(c => c.name);
    const nameWidth = Math.max(...colNames.map(n => n.length));
    rows.forEach((row, i) => {
      const cells = row.cells || row;
      console.log(`────── Row ${i + 1} ──────`);
      colNames.forEach((name, j) => {
        const v = cells[j] == null ? '<null>' : String(cells[j]);
        console.log(`  ${name.padEnd(nameWidth)} : ${v}`);
      });
      console.log('');
    });
    console.log(`  ${rows.length} row(s) returned.`);
    return;
  }

  const widths = cols.map((c, i) => {
    const header = String(c.name || '').length;
    const dataMax = rows.reduce((m, r) => {
      const cells = r.cells || r;
      const v = cells[i] == null ? '<null>' : String(cells[i]);
      return Math.max(m, v.length);
    }, 0);
    const w = Math.max(header, dataMax) + 2;
    return compact ? Math.min(w, 30) : w;
  });

  const renderCell = (s, w) => {
    const v = s == null ? '<null>' : String(s);
    return compact ? v.substring(0, w).padEnd(w) : v.padEnd(w);
  };

  const header = '  ' + cols.map((c, i) => renderCell(c.name, widths[i])).join('  ');
  const divider = '  ' + widths.map(w => '─'.repeat(w)).join('  ');
  console.log(header);
  console.log(divider);
  for (const r of rows) {
    const cells = r.cells || r;
    console.log('  ' + cells.map((v, i) => renderCell(v, widths[i])).join('  '));
  }
  const nextHint = (rows.length === (response.limit || 100)) ? `  (Use --offset ${offset + rows.length} for the next page.)` : '';
  console.log(`\n  ${rows.length} row(s) returned.${nextHint}`);
}
