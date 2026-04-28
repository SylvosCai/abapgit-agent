@Library('abapgit-agent-jenkins-lib') _

// All connection parameters are stored in Jenkins Managed Files (JSON),
// not in this repository. See:
//   Jenkins → abapgit-agent → Config Files → abapgit-agent-config

def agentUrl = scm?.userRemoteConfigs?.getAt(0)?.url ?: ''

configFileProvider([configFile(fileId: 'abapgit-agent-config', variable: 'CONFIG_FILE')]) {
    def cfg = readJSON file: env.CONFIG_FILE

    abapPipeline(
        credentialsId   : cfg.credentialsId,
        gitCredentialsId: cfg.gitCredentialsId,
        abapHost        : cfg.abapHost,
        abapPort        : cfg.abapPort ?: '443',
        abapClient      : cfg.abapClient ?: '100',
        abapLanguage    : cfg.abapLanguage ?: 'EN',
        abaplintConfig  : '.abaplint.json',
        lintScope       : 'changed',
        timeout         : 30,
        agentRepoUrl    : agentUrl,
        afterTests      : {
            withCredentials([usernamePassword(
                credentialsId: cfg.testGitCredentials,
                usernameVariable: 'TEST_GIT_USR',
                passwordVariable: 'TEST_GIT_PSW'
            )]) {
                sh """
                    printf 'machine %s login %s password %s\\n' '${cfg.gitHost}' "\$TEST_GIT_USR" "\$TEST_GIT_PSW" > "\$HOME/.netrc"
                    chmod 600 "\$HOME/.netrc"

                    npm install

                    ABAP_REF_DIR="\$HOME/abap-reference"
                    mkdir -p "\$ABAP_REF_DIR"

                    node -e "
                      var fs = require('fs');
                      var cfg = JSON.parse(fs.readFileSync('.abapGitAgent', 'utf8'));
                      cfg.referenceFolder = process.env.HOME + '/abap-reference';
                      cfg.testRepos = ${groovy.json.JsonOutput.toJson(cfg.testRepos)};
                      fs.writeFileSync('.abapGitAgent', JSON.stringify(cfg, null, 2));
                    "

                    abapgit-agent ref --clone SAP-samples/abap-cheat-sheets || true

                    npm run test:all
                """
            }
        }
    )
}
