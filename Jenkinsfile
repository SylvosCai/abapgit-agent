@Library('abapgit-agent-jenkins-lib') _

abapPipeline(
    credentialsId   : 'gze-100-abap-credentials',   // Jenkins credentials ID for ABAP system
    gitCredentialsId: 'PAT_grcaud-serviceuser',      // Jenkins credentials ID for git remote
    abapHost        : 'ldcigze.devsys.net.sap',      // SAP hostname
    abapPort        : '44300',                        // HTTPS port
    abapClient      : '100',                          // SAP client
    abapLanguage    : 'EN',                           // Logon language
    abaplintConfig  : '.abaplint-740.json',               // abaplint config for 7.40 branch
    lintScope       : 'changed',                      // only report issues in changed files (full scan for cross-ref correctness)
    timeout         : 30,                             // pipeline timeout in minutes
    agentRepoUrl    : 'https://github.tools.sap/I045696/abapgit-agent.git', // signals: install from workspace (npm link)
    afterTests      : {
        withCredentials([usernamePassword(
            credentialsId: 'i045696-personal-org-github-token',
            usernameVariable: 'TEST_GIT_USR',
            passwordVariable: 'TEST_GIT_PSW'
        )]) {
            sh """
                # Write ~/.netrc so test runners can clone/push to github.tools.sap/I045696/* repos.
                printf 'machine github.tools.sap login %s password %s\\n' "\$TEST_GIT_USR" "\$TEST_GIT_PSW" > "\$HOME/.netrc"
                chmod 600 "\$HOME/.netrc"

                # Install project dependencies for Jest tests
                npm install

                # Set up reference folder for 'ref' command tests
                ABAP_REF_DIR="\$HOME/abap-reference"
                mkdir -p "\$ABAP_REF_DIR"

                # Patch .abapGitAgent: add referenceFolder for ref command tests
                node -e "
                  var fs = require('fs');
                  var cfg = JSON.parse(fs.readFileSync('.abapGitAgent', 'utf8'));
                  cfg.referenceFolder = process.env.HOME + '/abap-reference';
                  fs.writeFileSync('.abapGitAgent', JSON.stringify(cfg, null, 2));
                "

                # Clone ABAP cheat sheets for ref --list-topics, ref --topic, etc.
                abapgit-agent ref --clone SAP-samples/abap-cheat-sheets || true

                # Run all tests
                npm run test:all
            """
        }
    }
)
