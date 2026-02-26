/**
 * Ref command - Search ABAP reference materials (cheat sheets and guidelines)
 */

module.exports = {
  name: 'ref',
  description: 'Search ABAP reference materials',
  requiresAbapConfig: false,
  requiresVersionCheck: false,

  async execute(args, context) {
    const refSearch = require('../src/ref-search');

    const topicIndex = args.indexOf('--topic');
    const cloneIndex = args.indexOf('--clone');
    const nameIndex = args.indexOf('--name');
    const listTopics = args.includes('--list-topics') || args.includes('-l');
    const listRepos = args.includes('--list-repos') || args.includes('-r');
    const jsonOutput = args.includes('--json');

    // Handle --clone option
    if (cloneIndex !== -1 && cloneIndex + 1 < args.length) {
      const repoUrl = args[cloneIndex + 1];
      const name = nameIndex !== -1 && nameIndex + 1 < args.length ? args[nameIndex + 1] : null;
      const result = refSearch.cloneRepository(repoUrl, name);
      if (jsonOutput) {
        console.log(JSON.stringify(result, null, 2));
      } else {
        refSearch.displayCloneResult(result);
      }
      return;
    }

    if (listRepos) {
      const result = await refSearch.listRepositories();
      if (jsonOutput) {
        console.log(JSON.stringify(result, null, 2));
      } else {
        refSearch.displayRepositories(result);
      }
      return;
    }

    if (listTopics) {
      const result = await refSearch.listTopics();
      if (jsonOutput) {
        console.log(JSON.stringify(result, null, 2));
      } else {
        refSearch.displayTopics(result);
      }
      return;
    }

    if (topicIndex !== -1 && topicIndex + 1 < args.length) {
      const topic = args[topicIndex + 1];
      const result = await refSearch.getTopic(topic);
      if (jsonOutput) {
        console.log(JSON.stringify(result, null, 2));
      } else {
        refSearch.displayTopic(result);
      }
      return;
    }

    // Pattern search (default)
    const patternIndex = args.findIndex((arg, idx) => idx > 0 && !arg.startsWith('--'));
    if (patternIndex === -1) {
      console.error('Error: No pattern specified');
      console.error('');
      console.error('Usage:');
      console.error('  abapgit-agent ref <pattern>           Search for pattern');
      console.error('  abapgit-agent ref --topic <name>      View specific topic');
      console.error('  abapgit-agent ref --list-topics       List available topics');
      console.error('  abapgit-agent ref --list-repos       List reference repositories');
      console.error('  abapgit-agent ref --clone <repo>     Clone a repository');
      console.error('');
      console.error('Examples:');
      console.error('  abapgit-agent ref "CORRESPONDING"');
      console.error('  abapgit-agent ref --topic exceptions');
      console.error('  abapgit-agent ref --list-topics');
      console.error('  abapgit-agent ref --list-repos');
      console.error('  abapgit-agent ref --clone SAP-samples/abap-cheat-sheets');
      console.error('  abapgit-agent ref --clone https://github.com/abapGit/abapGit.git');
      process.exit(1);
    }

    const pattern = args[patternIndex];
    const result = await refSearch.searchPattern(pattern);

    if (jsonOutput) {
      console.log(JSON.stringify(result, null, 2));
    } else {
      refSearch.displaySearchResults(result);
    }
  }
};
