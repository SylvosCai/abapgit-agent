const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
function fixture(value = { version: 1, systems: {} }) {
  const dir = fs.mkdtempSync(path.join(fs.realpathSync(os.tmpdir()), 'abgagt-catalog-'));
  fs.chmodSync(dir, 0o700);
  const file = path.join(dir, 'systems.json');
  const write = (target, data) => fs.writeFileSync(target, JSON.stringify(data), { mode: 0o600 });
  write(file, value);
  const previous = process.env.ABAP_SYSTEMS_PATH;
  process.env.ABAP_SYSTEMS_PATH = file;
  return { dir, file, write, read: () => JSON.parse(fs.readFileSync(file, 'utf8')),
    cleanup() { if (previous === undefined) delete process.env.ABAP_SYSTEMS_PATH; else process.env.ABAP_SYSTEMS_PATH = previous; fs.rmSync(dir, { recursive: true, force: true }); } };
}
const system = () => ({ protocol: 'https', host: 'sap.example', clients: {
  '100': { credentials: { provider: 'file', user: 'alice', password: ' private bytes \n' } },
  '200': { credentials: { provider: 'file', user: 'bob', password: '' } }
} });
module.exports = { fixture, system };
