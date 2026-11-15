const fs = require('fs');
const path = require('path');
const vm = require('vm');

afterEach(() => jest.restoreAllMocks());

test('SO10 has a built-in clone-only fixture URL without a config override', () => {
  jest.spyOn(fs, 'existsSync').mockReturnValue(false);
  jest.isolateModules(() => {
    expect(require('../integration/test-repos').getTestRepoUrl('so10'))
      .toBe('https://github.tools.sap/I045696/abgagt-so10-test.git');
  });
});

test('Jenkins forwards fixture overrides and optional read-only COPY settings', () => {
  const pipeline = fs.readFileSync(path.join(__dirname, '../../Jenkinsfile'), 'utf8');
  const script = pipeline.match(/node -e "([\s\S]*?)"/)[1];
  const config = { client: '123', user: 'test-user', password: 'synthetic-secret' };
  const testCfg = {
    testRepos: { so10: 'https://example.invalid/fixture.git' },
    testSo10: { copySource: { client: '000', key: { object: 'TEXT', name: 'EXISTING_FIXTURE', id: 'ST', language: 'E' } } }
  };
  const fakeFs = {
    readFileSync: jest.fn(file => JSON.stringify(file === '.abapGitAgent' ? config : testCfg)),
    writeFileSync: jest.fn()
  };
  vm.runInNewContext(script, { require: () => fakeFs, process: { env: { HOME: '/test-home' } } });
  expect(fakeFs.readFileSync).toHaveBeenCalledWith('.test-config.json', 'utf8');
  expect(JSON.parse(fakeFs.writeFileSync.mock.calls[0][1])).toEqual({
    ...config, ...testCfg, referenceFolder: '/test-home/abap-reference'
  });
  expect(pipeline).toContain('testSo10: testCfg.testSo10 ?: [:]');
});
