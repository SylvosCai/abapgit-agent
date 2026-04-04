/**
 * Drop command - Physically delete a single ABAP object from the ABAP system
 */

const { printHttpError } = require('../utils/format-error');

// Map file extension (second-to-last part) to ABAP object type label
const EXT_TO_TYPE = {
  clas: 'CLAS', intf: 'INTF', prog: 'PROG', fugr: 'FUGR',
  tabl: 'TABL', dtel: 'DTEL', ttyp: 'TTYP', doma: 'DOMA',
  ddls: 'DDLS', dcls: 'DCLS', msag: 'MSAG', stru: 'STRU'
};

/**
 * Derive a display label for the object from the file path.
 * e.g. "abap/zcl_foo.clas.abap" → { name: "ZCL_FOO", type: "CLAS" }
 */
function objectFromFile(filePath) {
  const base = filePath.split('/').pop();
  const parts = base.split('.');
  if (parts.length < 3) return null;
  const name = parts[0].toUpperCase();
  const typeExt = parts[1].toLowerCase();
  const type = EXT_TO_TYPE[typeExt] || typeExt.toUpperCase();
  return { name, type };
}

module.exports = {
  name: 'drop',
  description: 'Physically delete a single ABAP object from the ABAP system',
  requiresAbapConfig: true,
  requiresVersionCheck: true,

  async execute(args, context) {
    const { loadConfig, AbapHttp, gitUtils, getTransport, getConflictSettings } = context;

    // Show help if requested
    if (args.includes('--help') || args.includes('-h')) {
      console.log(`
Usage:
  abapgit-agent drop --files <file>
  abapgit-agent drop --files <file> --transport <TRANSPORT>
  abapgit-agent drop --files <file> --pull

Description:
  Physically deletes a single ABAP object from the ABAP system using abapGit's
  own object serializer. The object type and name are derived from the file path.

  Use --pull to immediately re-activate the object from git after deletion
  (useful to force a clean re-installation).

Parameters:
  --files <file>          Path to the ABAP source or XML file (required).
                          Accepted extensions: .abap, .asddls, .tabl.xml, etc.
                          The file must exist on disk.
  --transport <TRANSPORT> Transport request (e.g. DEVK900001). Optional.
  --pull                  Re-activate the object via pull after deletion.
  --conflict-mode <mode>  Conflict mode for --pull: abort (default) or ignore.

Examples:
  abapgit-agent drop --files abap/zcl_foo.clas.abap
  abapgit-agent drop --files abap/zcl_foo.clas.abap --pull
  abapgit-agent drop --files abap/zmy_table.tabl.xml --transport DEVK900001
`);
      return;
    }

    const filesArgIndex = args.indexOf('--files');
    const transportArgIndex = args.indexOf('--transport');
    const conflictModeArgIndex = args.indexOf('--conflict-mode');
    const doPull = args.includes('--pull');

    if (filesArgIndex === -1 || filesArgIndex + 1 >= args.length) {
      console.error('❌ Error: --files is required');
      console.error('   Usage: abapgit-agent drop --files <file>');
      process.exit(1);
    }

    const filePath = args[filesArgIndex + 1].trim();

    // Validate file exists on disk
    const fs = require('fs');
    if (!fs.existsSync(filePath)) {
      console.error(`❌ Error: file not found: ${filePath}`);
      process.exit(1);
    }

    // Validate file extension (same rules as pull --files)
    const base = filePath.split('/').pop();
    const parts = base.split('.');
    const lastExt = parts[parts.length - 1].toLowerCase();
    const ABAP_SOURCE_EXTS = new Set(['abap', 'asddls']);
    const isXmlOnlyObject = parts.length === 3 && parts[0].length > 0 && lastExt === 'xml';
    if (!ABAP_SOURCE_EXTS.has(lastExt) && !isXmlOnlyObject) {
      console.error('❌ Error: --files must be an ABAP source file (.abap, .asddls) or an XML-only object file (name.type.xml).');
      process.exit(1);
    }

    const obj = objectFromFile(filePath);

    if (obj && obj.type === 'DTEL') {
      console.error(`❌ drop does not support DTEL objects.`);
      console.error(`   Data elements cannot be re-activated after deletion due to SAP CBDA`);
      console.error(`   activation engine limitations. Edit the XML file and run pull instead.`);
      process.exit(1);
    }

    const transportRequest = transportArgIndex !== -1 ? args[transportArgIndex + 1] : (getTransport ? getTransport() : null);
    const conflictMode = conflictModeArgIndex !== -1 ? args[conflictModeArgIndex + 1] : (getConflictSettings ? getConflictSettings().mode : 'abort');

    console.log(`\n🗑️  Dropping ${obj ? obj.type + ' ' + obj.name : filePath} from ABAP system...`);
    if (transportRequest) {
      console.log(`   Transport: ${transportRequest}`);
    }

    const config = loadConfig();
    const http = new AbapHttp(config);

    try {
      const csrfToken = await http.fetchCsrfToken();

      const data = { file: filePath };
      if (transportRequest) {
        data.transport_request = transportRequest;
      }

      const result = await http.post('/sap/bc/z_abapgit_agent/drop', data, { csrfToken });

      const success = result.SUCCESS || result.success;
      const objectName = result.OBJECT || result.object;
      const objectType = result.TYPE || result.type;
      const message = result.MESSAGE || result.message;
      const error = result.ERROR || result.error;

      if (success === 'X' || success === true) {
        console.log(`✅ Object deleted successfully.`);
        if (objectName && objectType) {
          console.log(`   Object: ${objectName} (${objectType})`);
        }
      } else {
        console.error(`❌ Failed to delete object`);
        console.error(`   Error: ${error || message || 'Unknown error'}`);
        process.exit(1);
      }
    } catch (error) {
      printHttpError(error, {});
      process.exit(1);
    }

    // --pull: re-activate the object from git
    if (doPull) {
      console.log(`\n↩️  Re-pulling from git...`);
      const pullCommand = require('./pull');
      const gitUrl = gitUtils.getRemoteUrl();
      const branch = gitUtils.getBranch();
      if (!gitUrl) {
        console.error('❌ Cannot re-pull: no git remote configured.');
        process.exit(1);
      }
      try {
        await pullCommand.pull(
          gitUrl, branch, [filePath], transportRequest || null,
          loadConfig, AbapHttp, false, undefined, conflictMode, false, false
        );
      } catch (pullError) {
        // pull() already printed the error
        process.exit(1);
      }
    }
  }
};
