/**
 * Help command - Display usage information
 */

module.exports = {
  name: 'help',
  description: 'Display usage information and available commands',
  requiresAbapConfig: false,
  requiresVersionCheck: false,

  async execute(args, context) {
    const { versionCheck } = context;
    const version = versionCheck.getCliVersion();
    console.log(`
ABAP Git Agent v${version}

Usage:
  abapgit-agent <command> [options]

Commands:
  init --folder <folder> --package <package>
    Initialize local configuration for an existing git repository.
  init --update
    Update existing files (CLAUDE.md, copilot-instructions.md, guidelines) to latest version.

  create
    Create abapGit online repository in ABAP system.
    Auto-detects URL from git remote and package from .abapGitAgent.

  import [--message <message>]
    Import existing objects from package to git repository.
    Uses the git remote URL to find the abapGit online repository.

  pull [--url <git-url>] [--branch <branch>] [--files <file1,file2,...>] [--transport <request>]
    Pull and activate repository in ABAP system.
    Auto-detects git remote and branch from current directory.
    Use --files to pull only specific files.
    Use --transport to specify a transport request.

  inspect --files <file1>,<file2>,...
    Inspect ABAP source file(s) for issues. Currently runs syntax check.

  syntax --files <file1>,<file2>,... [--cloud] [--json]
    Check syntax of ABAP source files WITHOUT pull/activation.
    Reads source from local files and checks directly in ABAP system.
    Use --cloud for ABAP Cloud (BTP) stricter syntax checking.

  unit --files <file1>,<file2>,...
    Run AUnit tests for ABAP test class files (.testclasses.abap)

  run --program <NAME> | --class <NAME>
    Execute an ABAP program or class (IF_OO_ADT_CLASSRUN) and display its output.

  drop --files <file> [--pull] [--transport <TRANSPORT>] [--conflict-mode <mode>]
    Physically delete a single ABAP object from the ABAP system.
    Use --pull to immediately re-activate from git after deletion.

  lint [--files <file1,...>] [--base <branch>] [--config <file>]
    Run abaplint static analysis on changed ABAP files. Requires .abaplint.json.

  debug <subcommand> [options]
    Manage ABAP debugger breakpoints and attach debug sessions.
    Subcommands: set, list, delete, attach, step, vars, stack

  transport <subcommand> [options]
    List and manage SAP transport requests.
    Subcommands: list, create, release

  tree --package <package> [--depth <n>] [--include-types] [--json]
    Display package hierarchy tree from ABAP system

  list --package <package> [--type <types>] [--name <pattern>] [--limit <n>] [--offset <n>] [--json]
    List ABAP objects in a package with filtering and pagination

  view --objects <obj1>,<obj2>,... [--type <type>] [--json]
    View ABAP object definitions from the ABAP system

  preview --objects <obj1>,... [--type <type>] [--limit <n>] [--where <cond>] [--json]
    Preview data from ABAP tables or CDS views

  where --objects <obj1>,<obj2>,... [--type <type>] [--limit <n>] [--json]
    Find where-used list for ABAP objects (classes, interfaces, programs)

  dump [--user <user>] [--date <date>] [--time <HH:MM..HH:MM>] [--timezone <tz>] [--program <prog>] [--error <error>] [--limit <n>] [--detail <n>] [--json]
    Query short dumps (ST22) from ABAP system. Use --detail <n> to view full details.

  ref <pattern> [--json]
    Search ABAP reference repositories for patterns. Requires referenceFolder in .abapGitAgent.

  ref --topic <topic> [--json]
    View specific topic from cheat sheets (exceptions, sql, unit-tests, etc.)

  ref --list-topics
    List available topics for reference search

  ref --list-repos
    List all reference repositories in the reference folder

  ref --clone <repo> [--name <folder>]
    Clone a GitHub repository to the reference folder
    - Use full URL: https://github.com/user/repo.git
    - Or short name: user/repo or user/repo (assumes github.com)

  guide [--path] [--json]
    Show the full ABAP development guide bundled with the package.
    Reads directly from the installed npm package — always up-to-date.
    - Use --path to print only the file path (useful for pagers)
    - Use --json for machine-readable output

  guide --migrate [--dry-run] [--yes]
    Migrate repo from locally-copied guidelines to bundled fallback.
    Removes standard guideline files copied by old 'init', replaces full CLAUDE.md
    with slim stub. Project-specific files (*.local.md) are never removed.
    - Use --dry-run to preview changes without applying them
    - Use --yes to skip confirmation prompt

  upgrade [--abap-only] [--cli-only] [--version <v>]
    Upgrade CLI and/or ABAP backend to latest or specific version.

  health
    Check if ABAP REST API is healthy

  status
    Check if ABAP integration is configured for this repo

Examples:
  abapgit-agent init --folder /src --package ZMY_PACKAGE      # Initialize
  abapgit-agent init --update                                # Update files to latest
  abapgit-agent create                                       # Create repo
  abapgit-agent delete                                       # Delete repo
  abapgit-agent import                                       # Import objects to git
  abapgit-agent import --message "Initial import"            # Import with message
  abapgit-agent pull                                         # Auto-detect from git
  abapgit-agent pull --branch develop                        # Specific branch
  abapgit-agent pull --files src/zcl_my_class.clas.abap          # Specific file
  abapgit-agent pull --transport DEVK900001                  # With transport
  abapgit-agent inspect --files src/zcl_my_class.clas.abap       # Syntax check
  abapgit-agent syntax --files src/zcl_my_class.clas.abap        # Check without pull
  abapgit-agent syntax --files src/zmy_prog.prog.abap --cloud    # ABAP Cloud check
  abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap  # Run unit tests
  abapgit-agent run --program ZMY_REPORT                         # Execute report
  abapgit-agent run --class ZCL_MY_RUNNER                        # Execute class
  abapgit-agent drop --files src/zcl_foo.clas.abap --pull        # Drop and re-activate
  abapgit-agent lint                                             # abaplint on changed files
  abapgit-agent tree --package $MY_PACKAGE                       # Display package tree
  abapgit-agent list --package $MY_PACKAGE --type CLAS,INTF  # List classes & interfaces
  abapgit-agent view --objects ZCL_MY_CLASS                  # View class definition
  abapgit-agent preview --objects SFLIGHT                    # Preview table data
  abapgit-agent where --objects ZCL_MY_CLASS                 # Find where class is used
  abapgit-agent dump --date TODAY                            # Recent short dumps
  abapgit-agent dump --user DEVELOPER --detail 1             # Full detail of first result
  abapgit-agent ref "CORRESPONDING"                          # Search for pattern
  abapgit-agent ref --topic exceptions                       # View exceptions topic
  abapgit-agent guide                                        # Full ABAP dev guide
  abapgit-agent guide --path                                 # Path to guide file
  abapgit-agent health                                       # Health check
  abapgit-agent status                                       # Configuration status

For more info: https://github.com/SylvosCai/abapgit-agent
`);
  }
};
