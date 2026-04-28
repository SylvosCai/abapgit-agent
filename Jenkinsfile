@Library('abapgit-agent-jenkins-lib') _

// All connection parameters are configured in Jenkins job settings
// (Build with Parameters), not hardcoded here:
//   ABAP_HOST, ABAP_PORT, ABAP_CLIENT, ABAP_LANGUAGE
//   ABAP_CREDENTIALS_PARAM, GIT_CREDENTIALS_PARAM
//
// agentRepoUrl is auto-detected from the SCM configuration.

// Derive agentRepoUrl from SCM config so we don't hardcode the internal URL.
// This tells the pipeline to npm-link from the workspace instead of installing from npm.
def agentUrl = scm?.userRemoteConfigs?.getAt(0)?.url ?: ''

abapPipeline(
    abaplintConfig  : '.abaplint.json',
    lintScope       : 'changed',
    timeout         : 30,
    agentRepoUrl    : agentUrl,
    afterTests      : {
        withCredentials([usernamePassword(
            credentialsId: env.TEST_GIT_CREDENTIALS ?: 'test-git-credentials',
            usernameVariable: 'TEST_GIT_USR',
            passwordVariable: 'TEST_GIT_PSW'
        )]) {
            sh """
                # Write ~/.netrc so test runners can clone/push to test repos.
                # GIT_HOST must be set as a Jenkins environment variable.
                printf 'machine %s login %s password %s\\n' "\$GIT_HOST" "\$TEST_GIT_USR" "\$TEST_GIT_PSW" > "\$HOME/.netrc"
                chmod 600 "\$HOME/.netrc"

                # Install project dependencies for Jest tests
                npm install

                # Set up reference folder for 'ref' command tests
                ABAP_REF_DIR="\$HOME/abap-reference"
                mkdir -p "\$ABAP_REF_DIR"

                # Patch .abapGitAgent: add referenceFolder + testRepos from env
                node -e "
                  var fs = require('fs');
                  var cfg = JSON.parse(fs.readFileSync('.abapGitAgent', 'utf8'));
                  cfg.referenceFolder = process.env.HOME + '/abap-reference';
                  if (process.env.TEST_REPOS_JSON) {
                    cfg.testRepos = JSON.parse(process.env.TEST_REPOS_JSON);
                  }
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
