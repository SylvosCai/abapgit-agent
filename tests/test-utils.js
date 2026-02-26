#!/usr/bin/env node
/**
 * Quick test to verify utilities work correctly
 */

const gitUtils = require('../src/utils/git-utils');
const versionCheck = require('../src/utils/version-check');
const validators = require('../src/utils/validators');
const { loadConfig, isAbapIntegrationEnabled } = require('../src/config');

console.log('Testing utilities...\n');

// Test git-utils
console.log('1. Git Utils:');
console.log('   - isGitRepo:', gitUtils.isGitRepo());
console.log('   - getRemoteUrl:', gitUtils.getRemoteUrl());
console.log('   - getBranch:', gitUtils.getBranch());

// Test version-check
console.log('\n2. Version Check:');
console.log('   - getCliVersion:', versionCheck.getCliVersion());

// Test validators
console.log('\n3. Validators:');
console.log('   - convertDatesInWhereClause("FLDATE = \'2024-01-15\'"):',
  validators.convertDatesInWhereClause("FLDATE = '2024-01-15'"));
console.log('   - isValidPackageName("$MY_PACKAGE"):', validators.isValidPackageName('$MY_PACKAGE'));
console.log('   - isValidPackageName("ZINVALID PACKAGE"):', validators.isValidPackageName('ZINVALID PACKAGE'));
console.log('   - parseObjectFromFile("src/zcl_my_class.clas.abap"):',
  validators.parseObjectFromFile('src/zcl_my_class.clas.abap'));

// Test config
console.log('\n4. Config:');
console.log('   - isAbapIntegrationEnabled:', isAbapIntegrationEnabled());

if (isAbapIntegrationEnabled()) {
  const config = loadConfig();
  console.log('   - host:', config.host);
  console.log('   - client:', config.client);
}

console.log('\n✅ All utilities loaded successfully!');
