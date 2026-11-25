'use strict';

const schema = require('./central-catalog');

function options(args, allowed, positional = 0) {
  const result = { positionals: [], arg: [] };
  for (let i = 0; i < args.length; i++) {
    const token = args[i];
    if (!token.startsWith('-')) { result.positionals.push(token); continue; }
    const equal = token.indexOf('=');
    const key = token.slice(2, equal === -1 ? undefined : equal);
    if (!token.startsWith('--') || !allowed.includes(key)) throw new Error('Unsupported command option');
    if (key === 'json') {
      if (equal !== -1 || result.json) throw new Error('Invalid --json option');
      result.json = true;
      continue;
    }
    if (key !== 'arg' && schema.own(result, key)) throw new Error('Repeated command option');
    const value = equal === -1 ? args[++i] : token.slice(equal + 1);
    if (value === undefined || (equal === -1 && value.startsWith('-'))) throw new Error('Missing option value; use --arg=--flag for leading dashes');
    if (key === 'arg') result.arg.push(value);
    else result[key] = value;
  }
  if (result.positionals.length !== positional) throw new Error('Invalid command arguments');
  return result;
}
function provider(opts, catalog) {
  let value;
  if (opts.provider === 'file') {
    if (opts.adapter !== undefined || opts['item-id'] !== undefined || opts.arg.length) throw new Error('Conflicting provider options');
    value = { provider: 'file', user: opts.user, password: '' };
  } else if (opts.provider === 'bitwarden') {
    if (opts.user !== undefined || opts.adapter !== undefined || opts.arg.length) throw new Error('Conflicting provider options');
    value = { provider: 'bitwarden', itemId: opts['item-id'] };
  } else if (opts.provider === 'cli') {
    if (opts.user !== undefined || opts['item-id'] !== undefined) throw new Error('Conflicting provider options');
    value = { provider: 'cli', adapter: opts.adapter, args: opts.arg };
  } else throw new Error('Choose file, bitwarden or cli credentials');
  return schema.credentials(value, catalog);
}
const providerOptions = ['provider', 'user', 'adapter', 'item-id', 'arg', 'json'];
function output(opts, value, text) { console.log(opts.json ? JSON.stringify(value) : text); }
module.exports = { options, provider, providerOptions, output };
