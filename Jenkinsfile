@Library('abapgit-agent-jenkins-lib') _

// All connection parameters are stored in Jenkins Managed Files (JSON).
// See: Jenkins → abapgit-agent → Config Files → abapgit-agent-config

def agentUrl = scm?.userRemoteConfigs?.getAt(0)?.url ?: ''

// Load config — extract only primitive strings to avoid CPS LazyMap serialization issues
def cfgCredentialsId = ''
def cfgGitCredentialsId = ''
def cfgAbapHost = ''
def cfgAbapPort = '443'
def cfgAbapClient = '100'
def cfgAbapLanguage = 'EN'
def cfgTestGitCredentials = ''
def cfgGitHost = ''
def cfgFileId = 'd136001f-8e70-44a5-b0a3-b9c9711ad4c4'

node {
    configFileProvider([configFile(fileId: cfgFileId, variable: 'CONFIG_FILE')]) {
        def cfg = readJSON file: CONFIG_FILE
        cfgCredentialsId = cfg.credentialsId ?: ''
        cfgGitCredentialsId = cfg.gitCredentialsId ?: ''
        cfgAbapHost = cfg.abapHost ?: ''
        cfgAbapPort = cfg.abapPort ?: '443'
        cfgAbapClient = cfg.abapClient ?: '100'
        cfgAbapLanguage = cfg.abapLanguage ?: 'EN'
        cfgTestGitCredentials = cfg.testGitCredentials ?: ''
        cfgGitHost = cfg.gitHost ?: ''
    }
}

abapPipeline(
    credentialsId   : cfgCredentialsId,
    gitCredentialsId: cfgGitCredentialsId,
    abapHost        : cfgAbapHost,
    abapPort        : cfgAbapPort,
    abapClient      : cfgAbapClient,
    abapLanguage    : cfgAbapLanguage,
    abaplintConfig  : '.abaplint.json',
    lintScope       : 'changed',
    timeout         : 30,
    agentRepoUrl    : agentUrl,
    afterTests      : {
        configFileProvider([configFile(fileId: cfgFileId, variable: 'CONFIG_FILE')]) {
            def testCfg = readJSON file: CONFIG_FILE

            writeFile file: '.test-repos.json', text: groovy.json.JsonOutput.toJson(testCfg.testRepos ?: [:])

            withCredentials([usernamePassword(
                credentialsId: cfgTestGitCredentials,
                usernameVariable: 'TEST_GIT_USR',
                passwordVariable: 'TEST_GIT_PSW'
            )]) {
                sh """
                    printf 'machine %s login %s password %s\\n' '${cfgGitHost}' "\$TEST_GIT_USR" "\$TEST_GIT_PSW" > "\$HOME/.netrc"
                    chmod 600 "\$HOME/.netrc"

                    npm install

                    ABAP_REF_DIR="\$HOME/abap-reference"
                    mkdir -p "\$ABAP_REF_DIR"

                    node -e "
                      var fs = require('fs');
                      var cfg = JSON.parse(fs.readFileSync('.abapGitAgent', 'utf8'));
                      cfg.referenceFolder = process.env.HOME + '/abap-reference';
                      cfg.testRepos = JSON.parse(fs.readFileSync('.test-repos.json', 'utf8'));
                      fs.writeFileSync('.abapGitAgent', JSON.stringify(cfg, null, 2));
                    "
                    rm -f .test-repos.json

                    abapgit-agent ref --clone SAP-samples/abap-cheat-sheets || true

                    npm run test:all
                """
            }
        }
    }
)
