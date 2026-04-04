'use strict';

/**
 * run command — Execute an ABAP program or class and display its output.
 *
 * Programs (--program):
 *   POST /sap/bc/adt/programs/programrun/{name}
 *   Content-Type: application/vnd.sap.adt.programs.programRun+xml
 *   Accept: text/plain
 *   Note: SAP's ADT handler (CL_SEDI_ADT_PROGRAMRUN) calls SUBMIT with no
 *         WITH additions — runtime parameters cannot be passed. The program
 *         always runs with its coded defaults.
 *
 * Classes (--class):
 *   POST /sap/bc/adt/oo/classrun/{name}
 *   Accept: text/plain
 *   Requires the class to implement IF_OO_ADT_CLASSRUN.
 *   out->write() output is returned as plain text.
 */

const { AdtHttp } = require('../utils/adt-http');

const PROGRAM_RUN_XML =
  '<?xml version="1.0" encoding="UTF-8"?>\n' +
  '<adtprog:programRun xmlns:adtprog="http://www.sap.com/adt/programs/programs"/>';

module.exports = {
  name: 'run',
  description: 'Execute an ABAP program or class (IF_OO_ADT_CLASSRUN) and display its output',
  requiresAbapConfig: true,
  requiresVersionCheck: false,

  async execute(args, context) {
    const { loadConfig, getSafeguards } = context;

    if (args.includes('--help') || args.includes('-h')) {
      console.log(`
Usage:
  abapgit-agent run --program <NAME>
  abapgit-agent run --class <NAME>

Description:
  Execute an ABAP program or a class implementing IF_OO_ADT_CLASSRUN and display its output.

Parameters:
  --program <NAME>  Execute an ABAP report/program.
  --class <NAME>    Execute a class implementing IF_OO_ADT_CLASSRUN (run method).
  --json            Output as JSON.

Examples:
  abapgit-agent run --program ZMY_REPORT
  abapgit-agent run --class ZCL_MY_RUNNER
`);
      return;
    }

    // Check project-level safeguards
    const safeguards = getSafeguards();
    if (safeguards.disableRun) {
      console.error('❌ Error: run command is disabled for this project\n');
      if (safeguards.reason) {
        console.error(`Reason: ${safeguards.reason}\n`);
      }
      console.error('The run command has been disabled in .abapgit-agent.json');
      console.error('Please contact the project maintainer to enable it.');
      process.exit(1);
    }

    // Parse arguments
    let programName = null;
    let className = null;
    let jsonOutput = false;

    for (let i = 0; i < args.length; i++) {
      if (args[i] === '--program' && args[i + 1]) {
        programName = args[++i].toUpperCase();
      } else if (args[i] === '--class' && args[i + 1]) {
        className = args[++i].toUpperCase();
      } else if (args[i] === '--json') {
        jsonOutput = true;
      }
    }

    if (programName && className) {
      console.error('Error: --program and --class are mutually exclusive');
      process.exit(1);
    }

    if (!programName && !className) {
      console.error('Error: either --program or --class is required');
      console.error('Usage: abapgit-agent run --program <NAME>');
      console.error('       abapgit-agent run --class <NAME>');
      process.exit(1);
    }

    const config = loadConfig();
    const adt = new AdtHttp(config);

    if (!adt.csrfToken) {
      await adt.fetchCsrfToken();
    }

    let urlPath, body, requestOptions, targetName;

    if (className) {
      targetName = className;
      urlPath = `/sap/bc/adt/oo/classrun/${className}`;
      body = '';
      requestOptions = { accept: 'text/plain' };
    } else {
      targetName = programName;
      urlPath = `/sap/bc/adt/programs/programrun/${programName}`;
      body = PROGRAM_RUN_XML;
      requestOptions = {
        contentType: 'application/vnd.sap.adt.programs.programRun+xml',
        accept: 'text/plain'
      };
    }

    let response;
    try {
      response = await adt.request('POST', urlPath, body, requestOptions);
    } catch (err) {
      if (jsonOutput) {
        console.log(JSON.stringify({ success: false, target: targetName, error: err.message || String(err) }));
      } else {
        console.error(`Error: ${err.message || err}`);
      }
      process.exit(1);
    }

    const output = (response.body || '').trimEnd();

    if (jsonOutput) {
      const key = className ? 'class' : 'program';
      console.log(JSON.stringify({ success: true, [key]: targetName, output }));
    } else {
      console.log('\n--- Output ---');
      console.log(output || '(no output)');
      console.log('--------------');
      console.log(`✅ Completed: ${targetName}`);
    }
  }
};
