@Library('abapgit-agent-jenkins-lib') _

// All connection parameters are stored in Jenkins Managed Files (JSON).
// See: Jenkins → abapgit-agent → Config Files → abapgit-agent-config

def agentUrl = scm?.userRemoteConfigs?.getAt(0)?.url ?: ''
def cfgText = ''

// Load config from Jenkins Managed Files — requires a node context
node {
    configFileProvider([configFile(fileId: 'd136001f-8e70-44a5-b0a3-b9c9711ad4c4', variable: 'CONFIG_FILE')]) {
        cfgText = readFile(CONFIG_FILE)
    }
}

// Parse as regular HashMap (LazyMap from JsonSlurper is not serializable in Jenkins CPS)
def cfg = new HashMap(new groovy.json.JsonSlurper().parseText(cfgText))

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
        configFileProvider([configFile(fileId: 'd136001f-8e70-44a5-b0a3-b9c9711ad4c4', variable: 'CONFIG_FILE')]) {
            def testCfg = readJSON file: CONFIG_FILE

            // Write testRepos JSON to a temp file to avoid shell escaping issues
            writeFile file: '.test-repos.json', text: groovy.json.JsonOutput.toJson(testCfg.testRepos ?: [:])

            withCredentials([usernamePassword(
                credentialsId: testCfg.testGitCredentials,
                usernameVariable: 'TEST_GIT_USR',
                passwordVariable: 'TEST_GIT_PSW'
            )]) {
                sh """
                    printf 'machine %s login %s password %s\\n' '${testCfg.gitHost}' "\$TEST_GIT_USR" "\$TEST_GIT_PSW" > "\$HOME/.netrc"
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
