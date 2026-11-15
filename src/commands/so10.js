const fs = require('fs');
const { TextDecoder } = require('util');
const path = require('path');
const { validateLines, validatePayload, getRepository, readManagedFiles, readManaged, keyTuple, filename, writeManaged, readCommitted } = require('../utils/so10-files');
const { transportProfile } = require('../utils/so10-client-profile');

const isSuccessful = (result) => [true, 'X'].includes(result.SUCCESS ?? result.success);
const isTransportNumber = (value) => typeof value === 'string' && value.length === 10 && /^[A-Z0-9]{3}K[0-9]{6}$/.test(value);

module.exports = {
  name: 'so10',
  description: 'List, import, read, save, copy, drop and transport SO10 standard texts',
  requiresAbapConfig: true,
  requiresVersionCheck: false,

  async execute(args, context) {
    if (args.includes('--help') || args.includes('-h')) {
      console.log(`
Usage:
  abapgit-agent so10 list --prefix ZAPP_ [--prefix YAPP_] [--name <text-name>] [--id ST] [--language EN] [--max-hits 100] [--source-client 000] [--json]
  abapgit-agent so10 import --prefix ZAPP_ [--prefix YAPP_] [--source-client 000] [--max-hits 100] [--dry-run] [--adopt-system] [--json]
  abapgit-agent so10 read <text-name> --language EN [--source-client 000] [--id ST] [--json]
  abapgit-agent so10 create <text-name> --language EN [--file <path> | --itf-file <path>] [--id ST] [--json]
  abapgit-agent so10 update <text-name> --language EN [--file <path> | --itf-file <path>] [--conflict-mode abort|ignore] [--id ST] [--json]
  abapgit-agent so10 copy <text-name> --language EN [--source-client <source-client>] [--target-client <target-client>] [--overwrite] [--json]
  abapgit-agent so10 drop <text-name> --language EN [--id ST] [--target-client <approved-client>] [--json]
  abapgit-agent so10 transport <text-name> --language EN [--target-client <target-client>] [--transport DEVK900001] [--id ST] [--json]

List returns authorized headers only. Literal prefixes are mandatory; the combined default limit is 100, hard maximum 1000.
Overflow fails without returning keys or reading text bodies. LIST writes no files.
IMPORT requires tracked .abapgit-agent.json with so10.repoId. It writes version1 .itf.json files, then registers the rechecked source baseline.
--dry-run lists intended files without text reads or writes; --adopt-system replaces only clean tracked differing payloads.
IMPORT never stages, commits or pushes Git files, saves SO10 content, or records transports.
Transport records an existing text key on a customizing task; it does not save, copy or release texts.
Request resolution: --transport, config/environment, then the existing customizing hook/picker.
Copy reads and saves inside one authenticated backend session; client options never change the HTTP logon.
Copy source defaults to the configured development client; target defaults to customizingClient, then the configured client.
Resolved copy clients must differ. Transport uses --target-client, then customizingClient, then the configured client.
A different transport logon requires matching explicit customizingLogon credentials; no credentials are cloned.
An existing target is never replaced without --overwrite. Copy does not accept files.
Text name is the SO10 name/key. Text ID defaults to ST.
Create/update default to the full-key managed .itf.json payload in the tracked SO10-enabled repository.
Explicit --file (UTF-8) or --itf-file (ITF array/version1 payload) prepares clean managed desired work without Git commits.
Differing dirty work is never overwritten. Outside an enabled repository, a file and explicit --conflict-mode ignore are required.
Ignore does not bypass ownership/authentication/locking. A first CREATE saved without baseline recovery needs explicit IMPORT, not CREATE retry.
Saving never records a transport.
DROP requires a clean committed full-key ITF payload/configuration and matching owned SAP baseline/content.
It preserves Git files, reports deleted/baseline_removed separately, and accepts no standalone/ignore/delete alias.
DROP defaults to the configured client; --target-client is explicit native data selection, not a different HTTP logon.
`);
      return;
    }

    const jsonOutput = args.includes('--json');
    const positional = [];
    const options = {};
    const valueFlags = ['--language', '--id', '--file', '--itf-file', '--source-client', '--target-client', '--transport', '--prefix', '--max-hits', '--name', '--conflict-mode'];
    const report = (result) => {
      const success = isSuccessful(result);
      if (jsonOutput) console.log(JSON.stringify(result, null, 2));
      else if (!success) {
        console.error(`❌ ${result.ERROR || result.error || 'SO10 operation failed'}`);
        if (positional[0] === 'drop') console.error(`   Deleted: ${result.deleted ?? result.DELETED ?? 'unknown'}  Baseline removed: ${result.baseline_removed ?? result.BASELINE_REMOVED ?? 'unknown'}`);
        if (['create', 'update'].includes(positional[0])) {
          console.error(`   Saved: ${result.saved ?? result.SAVED ?? 'unknown'}  Baseline: ${result.baseline_recorded ?? result.BASELINE_RECORDED ?? 'unknown'}`);
          if (positional[0] === 'create' && [true, 'X'].includes(result.saved ?? result.SAVED)) console.error('   Recover with explicit IMPORT; do not retry CREATE.');
        }
      }
      else {
        console.log(`✅ ${result.MESSAGE || result.message || 'SO10 operation completed'}`);
        if (positional[0] === 'import') {
          console.log(`   Imported: ${result.imported}  Selected: ${result.selected}${result.dry_run ? ' (dry run)' : ''}`);
          console.log(JSON.stringify(result.files, null, 2));
          return;
        }
        if (positional[0] === 'list') {
          console.log(`   Headers: ${result.at_least}  Limit: ${result.max_hits}`);
          console.log(JSON.stringify(result.KEYS ?? result.keys ?? [], null, 2));
          return;
        }
        console.log(`   Text name: ${result.NAME ?? result.name}`);
        console.log(`   Text ID:   ${result.ID ?? result.id}`);
        console.log(`   Language:  ${result.LANGUAGE ?? result.language}`);
        if (positional[0] === 'drop') {
          console.log(`   Deleted: ${result.deleted ?? result.DELETED}  Baseline removed: ${result.baseline_removed ?? result.BASELINE_REMOVED}`);
        } else if (positional[0] === 'read') {
          console.log(JSON.stringify(result.LINES ?? result.lines ?? [], null, 2));
        } else if (positional[0] === 'transport') {
          console.log(`   Request: ${result.transport_request}  Task: ${result.transport_task}  Key: ${result.object_key}`);
        } else console.log('   Transport: not recorded');
      }
      if (!success) process.exitCode = 1;
    };

    let payload, repository, inputDocument, managedEntry;
    let fileWritten = false;
    try {
      for (let i = 0; i < args.length; i++) {
        const arg = args[i];
        if (arg === '--json') continue;
        if (['--overwrite', '--dry-run', '--adopt-system'].includes(arg)) {
          if (options[arg]) throw new Error(`Duplicate option: ${arg}`);
          options[arg] = true;
        } else if (valueFlags.includes(arg)) {
          if (arg !== '--prefix' && Object.hasOwn(options, arg)) throw new Error(`Duplicate option: ${arg}`);
          if (!args[i + 1] || args[i + 1].startsWith('--')) throw new Error(`Value required for ${arg}`);
          const value = args[++i];
          if (arg === '--prefix') (options[arg] ||= []).push(value);
          else options[arg] = value;
        } else if (arg.startsWith('-')) throw new Error(`Unknown option: ${arg}`);
        else positional.push(arg);
      }
      const [action, textName] = positional;
      const isList = ['list', 'import'].includes(action);
      if (positional.length !== (isList ? 1 : 2) || !['list', 'import', 'read', 'create', 'update', 'copy', 'drop', 'transport'].includes(action)) {
        throw new Error('Specify list/import with --prefix, or read, create, update, copy, drop or transport and one Text name');
      }
      if (!isList && ['--prefix', '--max-hits', '--name'].some((flag) => Object.hasOwn(options, flag))) {
        throw new Error('--prefix, --max-hits and --name are only valid for list/import');
      }
      if (action !== 'import' && (options['--dry-run'] || options['--adopt-system'])) {
        throw new Error('--dry-run and --adopt-system are only valid for import');
      }
      if (['import', 'drop'].includes(action)) { repository = getRepository(); readManagedFiles(repository); }
      const name = isList ? options['--name'] || '' : textName;
      if ((!isList || name) && (!name.trim() || name.length > 70 || /[\x00-\x1f\x7f*]/.test(name) || name.endsWith(' '))) {
        throw new Error('Text name must be an exact nonempty name of at most 70 characters');
      }
      const id = options['--id'] || (isList ? '' : 'ST');
      if (id && (id.length > 4 || /[^A-Za-z0-9_]/.test(id))) throw new Error('Text ID must contain 1–4 letters, digits or underscores');
      const language = options['--language'] || '';
      if ((!isList && !language) || language.length > 2 || /[^A-Za-z0-9]/.test(language)) {
        throw new Error('--language must be a SAP language key or ISO language code');
      }
      let maxHits;
      if (isList) {
        const prefixes = options['--prefix'];
        if (!prefixes?.length || prefixes.some((prefix) => !prefix.trim() || prefix.length > 70 || /[\x00-\x1f\x7f*+?#%]/.test(prefix))) {
          throw new Error('List requires at least one literal nonblank --prefix of at most 70 characters');
        }
        const limit = options['--max-hits'] || '100';
        maxHits = Number(limit);
        if (/[^0-9]/.test(limit) || !Number.isSafeInteger(maxHits) || maxHits < 1 || maxHits > 1000) {
          throw new Error('--max-hits must be an integer between 1 and 1000');
        }
      }
      const file = options['--file'];
      const itfFile = options['--itf-file'];
      const acceptsFile = ['create', 'update'].includes(action);
      if (file && itfFile || !acceptsFile && (file || itfFile)) {
        throw new Error(acceptsFile ? 'Specify only one --file or --itf-file' : 'Read/copy/transport do not accept content files');
      }
      if (options['--conflict-mode'] && (!acceptsFile || !['abort', 'ignore'].includes(options['--conflict-mode']))) {
        throw new Error('--conflict-mode abort|ignore is only valid for create/update');
      }
      if (acceptsFile) {
        repository = getRepository(process.cwd(), { optional: true });
        if (repository) readManagedFiles(repository);
        else if (!file && !itfFile) throw new Error('Outside an SO10-enabled repository, supply one --file or --itf-file and explicit --conflict-mode ignore');
      }
      if (options['--source-client'] && !['read', 'list', 'import', 'copy'].includes(action)) {
        throw new Error('--source-client is only valid for read/list/import/copy');
      }
      if (options['--target-client'] && !['copy', 'drop', 'transport'].includes(action)) {
        throw new Error('--target-client is only valid for copy/drop/transport');
      }
      if (action !== 'copy' && options['--overwrite']) throw new Error('--overwrite is only valid for copy');
      if (['--source-client', '--target-client'].some((flag) => options[flag] &&
        (options[flag].length !== 3 || /[^0-9]/.test(options[flag]))) ||
        (action === 'copy' && options['--target-client'] && options['--source-client'] === options['--target-client'])) {
        throw new Error('Source/target clients must contain exactly three digits; copy clients must differ');
      }
      if (options['--transport'] && action !== 'transport') {
        throw new Error('Saving/copying and transport recording are separate operations');
      }
      if (options['--transport'] && !isTransportNumber(options['--transport'])) {
        throw new Error('Invalid customizing request/task number');
      }
      payload = { action: action.toUpperCase(), object: 'TEXT', name, id: id.toUpperCase(), language: language.toUpperCase() };
      if (action === 'transport') payload.transport = options['--transport'];
      if (isList) { payload.prefixes = options['--prefix']; payload.max_hits = maxHits; }
      if (options['--source-client']) {
        if (action === 'copy') payload.source_client = options['--source-client'];
        else payload.client = options['--source-client'];
      }
      if (action === 'copy') {
        payload.target_client = options['--target-client'];
        payload.overwrite = Boolean(options['--overwrite']);
      }
      if (options['--conflict-mode']) payload.conflict_mode = options['--conflict-mode'];
      if (acceptsFile && (file || itfFile)) {
        let content;
        try {
          content = new TextDecoder('utf-8', { fatal: true }).decode(fs.readFileSync(file || itfFile));
        } catch (_) {
          throw new Error('Content file must be readable and valid UTF-8');
        }
        let lines;
        if (itfFile) {
          try { lines = JSON.parse(content); } catch (_) { throw new Error('ITF file must contain a JSON array or version1 SO10 payload'); }
          if (!Array.isArray(lines)) { inputDocument = validatePayload(lines); lines = inputDocument.lines; }
        } else {
          const text = content.replace(/\r\n|\r/g, '\n');
          const inputLines = text.split('\n');
          if (text.endsWith('\n')) inputLines.pop();
          lines = inputLines.map((tdline) => ({ tdformat: '*', tdline }));
        }
        validateLines(lines);
        payload.lines = lines;
      }
      if (acceptsFile && !repository && options['--conflict-mode'] !== 'ignore') {
        throw new Error('Standalone SO10 writes require explicit --conflict-mode ignore');
      }
    } catch (error) {
      report({ success: false, command: 'SO10', saved: false, transport_recorded: false,
        ...(positional[0] === 'drop' ? { action: 'DROP', deleted: false, baseline_removed: false, stage: 'VALIDATION' } : {}),
        ...(['create', 'update'].includes(positional[0]) ? { action: positional[0].toUpperCase(), git_managed: Boolean(repository),
          baseline_recorded: false, file_written: false, stage: 'VALIDATION' } : {}), error: error.message });
      return;
    }


    let writeAttempted = false;
    try {
      const baseConfig = context.loadConfig();
      const config = payload.action === 'TRANSPORT' ? transportProfile(baseConfig, options['--target-client']) : baseConfig;
      if (payload.action === 'COPY') {
        payload.source_client = payload.source_client || String(config.client);
        payload.target_client = payload.target_client || String(config.customizingClient ?? config.client);
        if ([payload.source_client, payload.target_client].some((client) => client.length !== 3 || /[^0-9]/.test(client)) ||
          payload.source_client === payload.target_client) {
          report({ success: false, command: 'SO10', action: 'COPY', saved: false, transport_recorded: false,
            error: 'Copy target must be a valid client different from its source' });
          return;
        }
      }
      const http = new context.AbapHttp(config);
      if (payload.action === 'TRANSPORT') {
        payload.transport = payload.transport || config.transport || process.env.ABAP_TRANSPORT;
        if (!payload.transport) {
          const { selectTransport, _getTransportHookConfig } = require('../utils/transport-selector');
          if (!jsonOutput || _getTransportHookConfig()?.hook) {
            payload.transport = await selectTransport(config, http, () => config, context.AbapHttp,
              context.getTransportSettings, 'customizing', { existingOnly: true });
          }
        }
        if (!isTransportNumber(payload.transport)) {
          report({ success: false, command: 'SO10', action: 'TRANSPORT', saved: false, transport_recorded: false,
            error: 'Resolve an existing customizing request/task with --transport, config/environment or the customizing hook' });
          return;
        }
      }
      const csrfToken = await http.fetchCsrfToken();
      if (payload.action === 'DROP') {
        const summary = { success: false, command: 'SO10', action: 'DROP', saved: false, deleted: false,
          baseline_removed: false, transport_recorded: false, stage: 'VALIDATION' };
        try {
          payload.target_client = options['--target-client'] || String(config.client);
          if (payload.target_client.length !== 3 || /[^0-9]/.test(payload.target_client)) throw new Error('DROP requires a three-digit target client');
          if (payload.language.length === 2) {
            const read = await http.post('/sap/bc/z_abapgit_agent/so10', { action: 'READ', object: payload.object,
              name: payload.name, id: payload.id, language: payload.language, client: payload.target_client }, { csrfToken });
            if ((!isSuccessful(read) && (read.error_kind ?? read.ERROR_KIND) !== 'NOT_FOUND') ||
                (read.object ?? read.OBJECT) !== payload.object || (read.name ?? read.NAME) !== payload.name ||
                (read.id ?? read.ID) !== payload.id || (read.data_client ?? read.DATA_CLIENT) !== payload.target_client) {
              throw new Error('DROP language normalization returned an invalid identity');
            }
            payload.language = read.language ?? read.LANGUAGE;
          }
          const key = { object: payload.object, name: payload.name, id: payload.id, language: payload.language };
          const committed = readCommitted(repository, key);
          Object.assign(summary, { file: committed.file, git_commit: committed.head });
          payload.lines = committed.payload.lines;
          payload.repo_id = repository.repoId;
          readCommitted(repository, key, committed.head);
          summary.stage = 'DROP';
          summary.deleted = null;
          summary.baseline_removed = null;
          const result = await http.post('/sap/bc/z_abapgit_agent/so10', payload, { csrfToken });
          Object.assign(summary, result, { saved: false, SAVED: false });
          if (isSuccessful(result) && (!([false, '', ' '].includes(result.saved ?? result.SAVED) &&
              [true, 'X'].includes(result.deleted ?? result.DELETED) &&
              [true, 'X'].includes(result.baseline_removed ?? result.BASELINE_REMOVED)) ||
              (result.object ?? result.OBJECT) !== key.object || (result.name ?? result.NAME) !== key.name ||
              (result.id ?? result.ID) !== key.id || (result.language ?? result.LANGUAGE) !== key.language ||
              (result.data_client ?? result.DATA_CLIENT) !== payload.target_client)) {
            Object.assign(summary, { success: false, SUCCESS: false, deleted: null, DELETED: null,
              baseline_removed: null, BASELINE_REMOVED: null, error: 'DROP acknowledgement is unverified; inspect text and baseline before retrying' });
          }
        } catch (_) {
          summary.error = summary.stage === 'DROP' ? 'DROP response is unknown; inspect text/baseline before retrying' :
            'DROP preparation failed; require a clean committed exact-key payload/configuration and valid selected client';
        }
        report(summary);
        return;
      }
      if (['CREATE', 'UPDATE'].includes(payload.action)) {
        try {
          if (payload.language.length === 2) {
            let observed;
            try { observed = await http.post('/sap/bc/z_abapgit_agent/so10', { action: 'READ', object: payload.object,
              name: payload.name, id: payload.id, language: payload.language }, { csrfToken }); }
            catch (_) { throw new Error('SO10 language normalization read failed; no save attempted'); }
            if (!observed || (!isSuccessful(observed) && (observed.error_kind ?? observed.ERROR_KIND) !== 'NOT_FOUND') ||
                (observed.object ?? observed.OBJECT) !== payload.object || (observed.name ?? observed.NAME) !== payload.name ||
                (observed.id ?? observed.ID) !== payload.id || (observed.data_client ?? observed.DATA_CLIENT) !== String(config.client)) {
              throw new Error('SO10 language normalization returned an invalid text/client identity');
            }
            payload.language = observed.language ?? observed.LANGUAGE;
          }
          const key = { object: payload.object, name: payload.name, id: payload.id, language: payload.language };
          keyTuple(key);
          if (inputDocument) validatePayload(inputDocument, key);
          if (repository) {
            if (payload.lines) {
              const prepared = writeManaged(repository, { version: 1, key, lines: payload.lines }, { replaceClean: true });
              fileWritten = prepared.fileWritten;
              managedEntry = readManaged(repository, key);
              if (managedEntry.file !== prepared.file || JSON.stringify(managedEntry.payload.lines.map(({ tdformat, tdline }) => [tdformat, tdline])) !==
                  JSON.stringify(payload.lines.map(({ tdformat, tdline }) => [tdformat, tdline]))) {
                throw new Error('Managed desired payload changed after preparation; no save attempted');
              }
            } else managedEntry = readManaged(repository, key);
            payload.lines = managedEntry.payload.lines;
            payload.repo_id = repository.repoId;
          }
        } catch (error) {
          fileWritten ||= error.fileWritten === true;
          report({ success: false, command: 'SO10', action: payload.action, saved: false, baseline_recorded: false,
            git_managed: Boolean(repository), file_written: fileWritten, transport_recorded: false, error: error.message });
          return;
        }
        writeAttempted = true;
        const result = await http.post('/sap/bc/z_abapgit_agent/so10', payload, { csrfToken });
        const summary = { ...result, git_managed: Boolean(repository), file_written: fileWritten,
          ...(managedEntry ? { file: managedEntry.file } : {}) };
        if (isSuccessful(result)) {
          const sourceSha = result.source_sha ?? result.SOURCE_SHA;
          const validIdentity = [true, 'X', false, ''].includes(result.saved ?? result.SAVED) &&
            (result.object ?? result.OBJECT) === payload.object && (result.name ?? result.NAME) === payload.name &&
            (result.id ?? result.ID) === payload.id && (result.language ?? result.LANGUAGE) === payload.language &&
            (result.data_client ?? result.DATA_CLIENT) === String(config.client);
          const validBaseline = repository ? [true, 'X'].includes(result.baseline_recorded ?? result.BASELINE_RECORDED) &&
            (result.base_repo_id ?? result.BASE_REPO_ID) === repository.repoId && (result.base_sha ?? result.BASE_SHA) === sourceSha &&
            typeof sourceSha === 'string' && sourceSha.length === 64 && /^[0-9a-f]{64}$/.test(sourceSha) :
            [false, ''].includes(result.baseline_recorded ?? result.BASELINE_RECORDED);
          if (!validIdentity || !validBaseline) Object.assign(summary, { success: false, SUCCESS: false, saved: null, SAVED: null,
            baseline_recorded: null, BASELINE_RECORDED: null, error: 'SO10 write identity/baseline acknowledgement is unverified; read back before retrying' });
        }
        report(summary);
        return;
      }
      if (payload.action === 'IMPORT') {
        const summary = { success: false, command: 'SO10', action: 'IMPORT', saved: false,
          transport_recorded: false, file_written: false, dry_run: Boolean(options['--dry-run']),
          selected: 0, imported: 0, files: [] };
        const request = async (body) => {
          try { return await http.post('/sap/bc/z_abapgit_agent/so10', body, { csrfToken }); }
          catch (_) { throw new Error('SO10 import request failed; inspect per-file outcomes before retrying'); }
        };
        try {
          const existing = readManagedFiles(repository);
          const dataClient = payload.client || String(config.client);
          const catalog = await request({ ...payload, action: 'LIST' });
          if (!isSuccessful(catalog)) throw new Error(catalog.error || catalog.ERROR || 'SO10 discovery failed');
          const keys = catalog.keys ?? catalog.KEYS;
          if (!Array.isArray(keys) || keys.length > payload.max_hits || [true, 'X'].includes(catalog.has_more ?? catalog.HAS_MORE)) {
            throw new Error('SO10 import discovery overflow or invalid headers; no bodies or files processed');
          }
          const tuples = keys.map(keyTuple);
          if (new Set(tuples).size !== keys.length || keys.some((key) => !payload.prefixes.some((prefix) => key.name.startsWith(prefix)))) {
            throw new Error('SO10 discovery returned duplicate or out-of-scope keys');
          }
          if ((catalog.data_client ?? catalog.DATA_CLIENT) !== dataClient) throw new Error('SO10 discovery data client mismatch');
          summary.selected = keys.length;
          for (const key of keys) {
            const item = { key, file: existing.get(keyTuple(key))?.file || path.join(repository.folder, filename(key)),
              file_written: false, baseline_recorded: false };
            summary.files.push(item);
            if (summary.dry_run) continue;
            try {
              const read = await request({ action: 'READ', ...key, client: dataClient });
              if (!isSuccessful(read)) throw new Error(read.error || read.ERROR || 'SO10 source read failed');
              const sourceKey = { object: read.object ?? read.OBJECT, name: read.name ?? read.NAME,
                id: read.id ?? read.ID, language: read.language ?? read.LANGUAGE };
              const document = validatePayload({ version: 1, key: sourceKey, lines: read.lines ?? read.LINES }, key);
              const sha = read.source_sha ?? read.SOURCE_SHA;
              if (typeof sha !== 'string' || !/^[0-9a-f]{64}$/.test(sha) || sha.length !== 64 ||
                  (read.data_client ?? read.DATA_CLIENT) !== dataClient) throw new Error('SO10 source identity/fingerprint is invalid');
              if ([true, 'X'].includes(read.base_found ?? read.BASE_FOUND) &&
                  (read.base_repo_id ?? read.BASE_REPO_ID) !== repository.repoId) throw new Error('SO10 source is owned by another repository');
              const prepared = writeManaged(repository, document, { replaceClean: Boolean(options['--adopt-system']) });
              item.file = prepared.file;
              item.file_written = prepared.fileWritten;
              const current = readManaged(repository, key);
              if (current.file !== prepared.file || current.payload.lines.length !== document.lines.length ||
                  current.payload.lines.some((line, index) => line.tdformat !== document.lines[index].tdformat || line.tdline !== document.lines[index].tdline)) {
                throw new Error('Managed payload changed after preparation; source baseline not registered');
              }
              item.baseline_recorded = null;
              const adopted = await request({ action: 'ADOPT', ...key, client: dataClient,
                repo_id: repository.repoId, source_sha: sha });
              item.baseline_recorded = adopted.baseline_recorded ?? adopted.BASELINE_RECORDED ?? null;
              if (!isSuccessful(adopted) || ![true, 'X'].includes(item.baseline_recorded)) {
                throw new Error(adopted.error || adopted.ERROR || 'SO10 source baseline was not verified; repeat explicit IMPORT');
              }
              if ((adopted.data_client ?? adopted.DATA_CLIENT) !== dataClient ||
                  (adopted.base_repo_id ?? adopted.BASE_REPO_ID) !== repository.repoId ||
                  (adopted.base_sha ?? adopted.BASE_SHA) !== sha) {
                item.baseline_recorded = null;
                throw new Error('SO10 adoption identity/baseline could not be verified');
              }
              summary.imported++;
            } catch (error) {
              item.file_written ||= error.fileWritten === true;
              item.error = error.message;
              throw error;
            } finally {
              summary.file_written ||= item.file_written;
            }
          }
          summary.success = true;
          summary.message = summary.dry_run ? 'SO10 import preview; no bodies read or files/baselines written' : 'SO10 sources imported; no texts saved or Git commits made';
        } catch (error) {
          summary.error = error.message || 'SO10 import failed; inspect per-file outcomes before retrying';
        }
        report(summary);
        return;
      }
      writeAttempted = !['READ', 'LIST'].includes(payload.action);
      const result = await http.post('/sap/bc/z_abapgit_agent/so10', payload, { csrfToken });
      report(result);
    } catch (_) {
      report({ success: false, command: 'SO10', action: payload.action, name: payload.name,
        ...(payload.action === 'COPY' ? { source_client: payload.source_client, target_client: payload.target_client } : {}),
        ...(payload.action === 'DROP' ? { deleted: false, baseline_removed: false } : {}),
        saved: ['TRANSPORT', 'IMPORT', 'DROP'].includes(payload.action) ? false : writeAttempted ? null : false,
        ...(['CREATE', 'UPDATE'].includes(payload.action) ? { git_managed: Boolean(repository), file_written: fileWritten,
          baseline_recorded: writeAttempted ? null : false } : {}),
        transport_recorded: payload.action === 'TRANSPORT' && writeAttempted ? null : false,
        error: payload.action === 'TRANSPORT' ? (writeAttempted ?
          'SO10 transport request failed; recording outcome may be unknown. Check the request before retrying.' :
          'SO10 transport preparation failed; no recording was attempted') : writeAttempted ?
          'SO10 save request failed; the save outcome may be unknown. Read back before retrying.' :
          'SO10 read or copy preparation failed; no save was attempted' });
    }
  }
};
