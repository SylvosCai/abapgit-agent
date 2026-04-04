'use strict';

/**
 * Dump command - Query short dumps (ST22) from ABAP system
 */

// Convert a local date/time in the given IANA timezone to a UTC Date.
// Uses an iterative approach to reliably handle DST transitions.
function localToUTC(dateStr, timeStr, timezone) {
  const iso = `${dateStr.slice(0,4)}-${dateStr.slice(4,6)}-${dateStr.slice(6,8)}` +
              `T${timeStr.slice(0,2)}:${timeStr.slice(2,4)}:${timeStr.slice(4,6)}`;
  let candidate = new Date(iso + 'Z');
  for (let i = 0; i < 3; i++) {
    const localStr = candidate.toLocaleString('sv-SE', { timeZone: timezone }).replace(' ', 'T');
    const diff = new Date(iso + 'Z').getTime() - new Date(localStr + 'Z').getTime();
    candidate = new Date(candidate.getTime() + diff);
  }
  return candidate;
}

// Format a UTC Date as a local date/time in the given IANA timezone.
// Returns { date: 'YYYY-MM-DD', time: 'HH:MM:SS' }.
function utcToLocal(utcDate, timezone) {
  const local = utcDate.toLocaleString('sv-SE', { timeZone: timezone });
  const [date, time] = local.split(' ');
  return { date, time };
}

// Parse a 14-char UTC timestamp string (YYYYMMDDhhmmss) to a Date.
function parseUTCTimestamp(ts) {
  const s = String(ts).padStart(14, '0');
  return new Date(`${s.slice(0,4)}-${s.slice(4,6)}-${s.slice(6,8)}T${s.slice(8,10)}:${s.slice(10,12)}:${s.slice(12,14)}Z`);
}

// Format a UTC Date as a 14-char timestamp string (YYYYMMDDhhmmss).
function toTimestampStr(date) {
  return date.toISOString().replace(/[-T:Z]/g, '').slice(0, 14);
}

// Parse the user's --date argument into { from, to } as YYYYMMDD strings,
// respecting the given timezone for TODAY/YESTERDAY keywords.
function parseDateArg(dateStr, timezone) {
  const nowLocal = utcToLocal(new Date(), timezone);
  const todayStr = nowLocal.date.replace(/-/g, '');

  const upper = dateStr.toUpperCase();
  if (upper === 'TODAY') {
    return { from: todayStr, to: todayStr };
  }
  if (upper === 'YESTERDAY') {
    const d = new Date(nowLocal.date + 'T00:00:00Z');
    d.setUTCDate(d.getUTCDate() - 1);
    const yStr = d.toISOString().slice(0, 10).replace(/-/g, '');
    return { from: yStr, to: yStr };
  }
  if (dateStr.includes('..')) {
    const [a, b] = dateStr.split('..');
    return { from: a.replace(/-/g, ''), to: b.replace(/-/g, '') };
  }
  const d = dateStr.replace(/-/g, '');
  return { from: d, to: d };
}

function parseTimeArg(timeStr) {
  const padTime = (t) => (t.replace(/:/g, '') + '000000').substring(0, 6);
  if (timeStr.includes('..')) {
    const [a, b] = timeStr.split('..');
    return { from: padTime(a), to: padTime(b) };
  }
  const t = padTime(timeStr);
  return { from: t, to: t };
}

// Resolve display date/time for a dump entry, using utc_timestamp when available.
function resolveDateTime(dump, timezone) {
  const ts = dump.UTC_TIMESTAMP || dump.utc_timestamp;
  if (ts && timezone) {
    const utcDate = parseUTCTimestamp(ts);
    return utcToLocal(utcDate, timezone);
  }
  return {
    date: dump.DATE || dump.date || '',
    time: dump.TIME || dump.time || '',
  };
}

function renderList(dumps, total, limit, timezone) {
  const totalNum = Number(total) || dumps.length;
  const tzLabel  = timezone ? ` [${timezone}]` : '';
  console.log(`\n  Short Dumps (${totalNum} found)${tzLabel}\n`);

  if (dumps.length === 0) {
    console.log('  No short dumps found for the given filters.\n');
    return;
  }

  const W = { num: 3, date: 10, time: 8, user: 12, prog: 30, err: 40 };
  const hdr = `  ${'#'.padStart(W.num)}  ${'Date'.padEnd(W.date)}  ${'Time'.padEnd(W.time)}  ${'User'.padEnd(W.user)}  ${'Program'.padEnd(W.prog)}  Error`;
  const sep = '  ' + '-'.repeat(W.num + 2 + W.date + 2 + W.time + 2 + W.user + 2 + W.prog + 2 + W.err);
  console.log(hdr);
  console.log(sep);

  dumps.forEach((d, i) => {
    const { date, time } = resolveDateTime(d, timezone);
    const user = (d.USER || d.user || '').substring(0, W.user);
    const prog = (d.PROGRAM || d.program || '').substring(0, W.prog);
    const err  = (d.ERROR || d.error || '');
    console.log(`  ${String(i + 1).padStart(W.num)}  ${date.padEnd(W.date)}  ${time.padEnd(W.time)}  ${user.padEnd(W.user)}  ${prog.padEnd(W.prog)}  ${err}`);
  });

  console.log('');
  if (totalNum > limit) {
    console.log(`  Showing ${dumps.length} of ${totalNum} dumps. Use --limit to see more.`);
  }
  console.log('  Use --detail <number> to see full dump details\n');
}

function renderDetail(dump, timezone) {
  console.log('\n  Short Dump Detail\n');

  const { date, time } = resolveDateTime(dump, timezone);
  const tzLabel = timezone ? ` (${timezone})` : '';

  const fields = [
    ['Error',     dump.ERROR     || dump.error     || ''],
    ['Date',      date + tzLabel],
    ['Time',      time],
    ['User',      dump.USER      || dump.user      || ''],
    ['Program',   dump.PROGRAM   || dump.program   || ''],
    ['Object',    dump.OBJECT    || dump.object    || ''],
    ['Package',   dump.PACKAGE   || dump.package   || ''],
    ['Exception', dump.EXCEPTION || dump.exception || ''],
  ];
  fields.forEach(([label, val]) => {
    if (val) {
      console.log(`  ${label.padEnd(12)}${val}`);
    }
  });

  const normalize = (s) => s.replace(/\\n/g, '\n').replace(/\\r/g, '');
  const whatHappened  = normalize(dump.WHAT_HAPPENED  || dump.what_happened  || '');
  const errorAnalysis = normalize(dump.ERROR_ANALYSIS || dump.error_analysis || '');

  if (whatHappened) {
    console.log('\n  What happened:');
    console.log('  ' + '-'.repeat(55));
    console.log(whatHappened.split('\n').map(l => '  ' + l).join('\n'));
  }
  if (errorAnalysis) {
    console.log('\n  Error analysis:');
    console.log('  ' + '-'.repeat(55));
    console.log(errorAnalysis.split('\n').map(l => '  ' + l).join('\n'));
  }

  const stack = dump.CALL_STACK || dump.call_stack || [];

  // Separate structured frames (have level) from source block (no level, has raw text)
  const frames     = stack.filter(f => (f.LEVEL || f.level));
  const srcEntries = stack.filter(f => !(f.LEVEL || f.level) && (f.METHOD || f.method));

  if (frames.length > 0) {
    console.log('\n  Call stack:');
    console.log('  ' + '-'.repeat(55));
    frames.forEach((frame) => {
      const level   = frame.LEVEL   || frame.level   || '';
      const cls     = frame.CLASS   || frame.class   || '';
      const method  = frame.METHOD  || frame.method  || '';
      const program = frame.PROGRAM || frame.program || '';
      const include = frame.INCLUDE || frame.include || '';
      const line    = frame.LINE    || frame.line    || '';
      const location = cls ? `${cls}->${method}` : (method ? `${program} ${method}` : program || include);
      const lineRef  = line ? ` (line ${line})` : '';
      console.log(`  ${String(level).padStart(3)}  ${location}${lineRef}`);
    });
  }

  if (srcEntries.length > 0) {
    const srcInc  = dump.SOURCE_INCLUDE || dump.source_include || '';
    const srcLine = dump.SOURCE_LINE    || dump.source_line    || 0;
    const heading = srcInc ? `  Source (${srcInc}, line ${srcLine}):` : '  Source:';
    console.log('\n' + heading);
    console.log('  ' + '-'.repeat(55));
    const rawText = srcEntries[0].METHOD || srcEntries[0].method || '';
    console.log(normalize(rawText).split('\n').map(l => '  ' + l).join('\n'));
  }

  console.log('');
}

module.exports = {
  name: 'dump',
  description: 'Query short dumps (ST22) from ABAP system',
  requiresAbapConfig: true,
  requiresVersionCheck: false,

  async execute(args, context) {
    const { loadConfig, AbapHttp } = context;

    if (args.includes('--help') || args.includes('-h')) {
      console.log(`
Usage:
  abapgit-agent dump [--user <user>] [--date <date>] [--time <HH:MM..HH:MM>]
                     [--program <prog>] [--error <error>] [--limit <n>] [--detail <n>] [--json]

Description:
  Query short dumps (ST22) from the ABAP system.

Parameters:
  --user <user>           Filter by user name.
  --date <date>           Filter by date: TODAY, YESTERDAY, or YYYYMMDD or YYYYMMDD..YYYYMMDD.
  --time <HH:MM..HH:MM>   Filter by time range (e.g. 08:00..17:00).
  --program <prog>        Filter by program name.
  --error <error>         Filter by error type (e.g. RABAX_STATE).
  --timezone <tz>         Timezone for date/time filter (default: system timezone).
  --limit <n>             Maximum number of results (default: 10).
  --detail <n>            Show full detail of the Nth result (1-based).
  --json                  Output as JSON.

Examples:
  abapgit-agent dump
  abapgit-agent dump --date TODAY
  abapgit-agent dump --user DEVELOPER
  abapgit-agent dump --date TODAY --limit 20
  abapgit-agent dump --user DEVELOPER --detail 1
`);
      return;
    }

    const idx = (flag) => args.indexOf(flag);
    const val = (flag) => {
      const i = idx(flag);
      return i !== -1 && i + 1 < args.length ? args[i + 1] : null;
    };

    const userRaw    = val('--user');
    const dateRaw    = val('--date');
    const timeRaw    = val('--time');
    const programRaw = val('--program');
    const errorRaw   = val('--error');
    const limitRaw   = val('--limit');
    const detailRaw  = val('--detail');
    const timezoneRaw = val('--timezone');
    const jsonOutput = args.includes('--json');

    const user    = userRaw    ? userRaw.toUpperCase()    : null;
    const program = programRaw ? programRaw.toUpperCase() : null;
    const error   = errorRaw   ? errorRaw.toUpperCase()   : null;

    // Resolve timezone: explicit flag > system default
    const timezone = timezoneRaw || Intl.DateTimeFormat().resolvedOptions().timeZone;

    let limit = 20;
    if (limitRaw) {
      const parsed = parseInt(limitRaw, 10);
      if (!isNaN(parsed) && parsed > 0) {
        limit = Math.min(parsed, 100);
      }
    }

    // Build UTC timestamp range from user's date/time in their timezone
    let tsFrom = null, tsTo = null;
    if (dateRaw) {
      const dates = parseDateArg(dateRaw, timezone);
      const timeF = timeRaw ? parseTimeArg(timeRaw).from : '000000';
      const timeT = timeRaw ? parseTimeArg(timeRaw).to   : '235959';
      tsFrom = toTimestampStr(localToUTC(dates.from, timeF, timezone));
      tsTo   = toTimestampStr(localToUTC(dates.to,   timeT, timezone));
    } else if (timeRaw) {
      // Time filter without date: apply to the default 7-day window
      const now    = utcToLocal(new Date(), timezone);
      const today  = now.date.replace(/-/g, '');
      const sevenAgo = (() => {
        const d = new Date(now.date + 'T00:00:00Z');
        d.setUTCDate(d.getUTCDate() - 7);
        return d.toISOString().slice(0, 10).replace(/-/g, '');
      })();
      const times = parseTimeArg(timeRaw);
      tsFrom = toTimestampStr(localToUTC(sevenAgo, times.from, timezone));
      tsTo   = toTimestampStr(localToUTC(today,    times.to,   timezone));
    }

    const detailN = detailRaw ? parseInt(detailRaw, 10) : null;

    const config = loadConfig();
    const http   = new AbapHttp(config);
    const csrfToken = await http.fetchCsrfToken();

    const buildData = (extra) => {
      const data = { limit };
      if (user)    data.user    = user;
      if (program) data.program = program;
      if (error)   data.error   = error;
      if (tsFrom) {
        data.ts_from = tsFrom;
        data.ts_to   = tsTo;
      }
      return Object.assign(data, extra);
    };

    if (detailN) {
      // Two-step: first list (using limit 100 to get full page), then detail
      const listResult = await http.post('/sap/bc/z_abapgit_agent/dump', buildData({ limit: 100 }), { csrfToken });
      const listSuccess = listResult.SUCCESS || listResult.success;
      const listErr     = listResult.ERROR   || listResult.error;

      if (!listSuccess || listErr) {
        console.error(`\n  Error: ${listErr || 'Failed to query short dumps'}\n`);
        return;
      }

      const dumps = listResult.DUMPS || listResult.dumps || [];
      if (detailN < 1 || detailN > dumps.length) {
        console.error(`\n  Error: Row number ${detailN} not found in results (found ${dumps.length} dump(s))\n`);
        return;
      }

      const targetId = dumps[detailN - 1].ID || dumps[detailN - 1].id;
      const detailResult = await http.post('/sap/bc/z_abapgit_agent/dump', buildData({ detail: targetId }), { csrfToken });

      const success = detailResult.SUCCESS || detailResult.success;
      const errMsg  = detailResult.ERROR   || detailResult.error;

      if (!success || errMsg) {
        console.error(`\n  Error: ${errMsg || 'Failed to load dump detail'}\n`);
        return;
      }

      if (jsonOutput) {
        console.log(JSON.stringify(detailResult, null, 2));
        return;
      }

      const detailDumps = detailResult.DUMPS || detailResult.dumps || [];
      renderDetail(detailDumps[0] || {}, timezone);
      return;
    }

    // List mode
    const result  = await http.post('/sap/bc/z_abapgit_agent/dump', buildData({}), { csrfToken });
    const success = result.SUCCESS || result.success;
    const errMsg  = result.ERROR   || result.error;

    if (!success || errMsg) {
      console.error(`\n  Error: ${errMsg || 'Failed to query short dumps'}\n`);
      return;
    }

    if (jsonOutput) {
      console.log(JSON.stringify(result, null, 2));
      return;
    }

    const dumps = result.DUMPS  || result.dumps  || [];
    const total = result.TOTAL  || result.total  || dumps.length;
    renderList(dumps, total, limit, timezone);
  }
};
