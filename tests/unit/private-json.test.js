const describe = process.platform === 'win32' ? global.describe.skip : global.describe;
const fs = require('node:fs');
const path = require('node:path');
const { fixture } = require('../helpers/central-catalog');
const { readPrivate, writePrivate } = require('../../src/utils/private-json');
const { readCatalog } = require('../../src/utils/central-catalog');
describe('private catalog persistence', () => {
  let f;
  beforeEach(() => { f = fixture(); });
  afterEach(() => f.cleanup());
  test('same-directory atomic replacement preserves mode and changes identity', () => {
    const before = readPrivate(f.file);
    writePrivate(f.file, { version: 1, systems: {}, keep: true }, before);
    expect(readPrivate(f.file).value.keep).toBe(true);
    expect(readPrivate(f.file).identity).not.toBe(before.identity);
    expect(fs.statSync(f.file).mode & 0o777).toBe(0o600);
    expect(fs.readdirSync(f.dir)).toEqual(['systems.json']);
  });
  test('concurrent changes and competing locks never overwrite existing data', () => {
    const before = readPrivate(f.file);
    f.write(f.file, { version: 1, changed: true });
    expect(() => writePrivate(f.file, { destroyed: true }, before)).toThrow('concurrently');
    expect(readPrivate(f.file).value.changed).toBe(true);
    f.write(`${f.file}.lock`, {});
    expect(() => writePrivate(f.file, {}, readPrivate(f.file))).toThrow('being edited');
    expect(readPrivate(f.file).value.changed).toBe(true);
  });
  test('rejects symlinks, hardlinks, public files and non-private catalog directories', () => {
    const link = path.join(f.dir, 'link'); fs.symlinkSync(f.file, link);
    expect(() => readPrivate(link)).toThrow('Unsafe');
    fs.unlinkSync(link); fs.linkSync(f.file, link);
    expect(() => readPrivate(f.file)).toThrow('owner-only'); fs.unlinkSync(link);
    fs.chmodSync(f.file, 0o644); expect(() => readPrivate(f.file)).toThrow('owner-only'); fs.chmodSync(f.file, 0o600);
    fs.chmodSync(f.dir, 0o755); expect(() => readCatalog()).toThrow('directory');
  });
  test('JSON errors do not expose parser excerpts or secrets', () => {
    fs.writeFileSync(f.file, '{"password":"private-marker" invalid');
    expect(() => readPrivate(f.file)).toThrow('Invalid configuration JSON');
    try { readPrivate(f.file); } catch (e) { expect(e.message).not.toContain('private-marker'); }
  });
  test('write failures leave the original and clean up temporary files', () => {
    const before = readPrivate(f.file);
    const spy = jest.spyOn(fs, 'renameSync').mockImplementation(() => { throw Object.assign(new Error('private-marker'), { code: 'EACCES' }); });
    try {
      expect(() => writePrivate(f.file, { changed: true }, before)).toThrow('safely write');
      expect(readPrivate(f.file).text).toBe(before.text);
      expect(fs.readdirSync(f.dir)).toEqual(['systems.json']);
    } finally { spy.mockRestore(); }
  });
});
