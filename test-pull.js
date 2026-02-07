const https = require('https');
const fs = require('fs');
const path = require('path');

const COOKIE_FILE = path.join(__dirname, '.abapgit_agent_cookies.txt');
const configPath = path.join(__dirname, '.abapGitAgent');
const config = JSON.parse(fs.readFileSync(configPath, 'utf8'));

function readNetscapeCookies() {
  if (!fs.existsSync(COOKIE_FILE)) return '';
  const content = fs.readFileSync(COOKIE_FILE, 'utf8');
  return content;
}

async function pull() {
  // Clear old cookies first (ignore if doesn't exist)
  if (fs.existsSync(COOKIE_FILE)) fs.unlinkSync(COOKIE_FILE);

  // First, authenticate and get initial cookies (GET /health which is supported)
  console.log('1. Initial auth via /health...');
  let initialCookies = '';
  await new Promise((resolve, reject) => {
    const options = {
      hostname: config.host,
      port: config.sapport,
      path: '/sap/bc/z_abapgit_agent/health',
      method: 'GET',
      headers: {
        'Authorization': 'Basic ' + Buffer.from(config.user + ':' + config.password).toString('base64'),
        'sap-client': config.client,
        'sap-language': config.language
      },
      agent: new https.Agent({ rejectUnauthorized: false })
    };

    const req = https.request(options, (res) => {
      const setCookie = res.headers['set-cookie'];
      if (setCookie) {
        initialCookies = setCookie.map(c => c.split(';')[0]).join('; ');
        fs.writeFileSync(COOKIE_FILE, initialCookies);
      }
      let body = '';
      res.on('data', chunk => body += chunk);
      res.on('end', () => {
        console.log('   Status:', res.statusCode);
        console.log('   Cookies:', initialCookies);
        resolve();
      });
    });
    req.on('error', reject);
    req.end();
  });

  // Now fetch CSRF token with the session cookie
  console.log('\n2. Fetch CSRF token with session cookie...');
  const token = await new Promise((resolve, reject) => {
    const options = {
      hostname: config.host,
      port: config.sapport,
      path: '/sap/bc/z_abapgit_agent/pull',
      method: 'GET',
      headers: {
        'Authorization': 'Basic ' + Buffer.from(config.user + ':' + config.password).toString('base64'),
        'sap-client': config.client,
        'sap-language': config.language,
        'X-CSRF-Token': 'fetch',
        'Cookie': initialCookies
      },
      agent: new https.Agent({ rejectUnauthorized: false })
    };

    const req = https.request(options, (res) => {
      console.log('   Status:', res.statusCode);
      console.log('   CSRF Token:', res.headers['x-csrf-token']);
      const setCookie = res.headers['set-cookie'];
      if (setCookie) {
        const newCookies = setCookie.map(c => c.split(';')[0]).join('; ');
        fs.writeFileSync(COOKIE_FILE, newCookies);
        console.log('   New Cookies:', newCookies);
      }
      let body = '';
      res.on('data', chunk => body += chunk);
      res.on('end', () => {
        resolve(res.headers['x-csrf-token']);
      });
    });
    req.on('error', reject);
    req.end();
  });

  console.log('\n3. Make POST request with latest cookies...');
  const latestCookies = fs.readFileSync(COOKIE_FILE, 'utf8');
  console.log('   Using cookies:', latestCookies.substring(0, 80) + '...');

  const postOptions = {
    hostname: config.host,
    port: config.sapport,
    path: '/sap/bc/z_abapgit_agent/pull',
    method: 'POST',
    headers: {
      'Authorization': 'Basic ' + Buffer.from(config.user + ':' + config.password).toString('base64'),
      'sap-client': config.client,
      'sap-language': config.language,
      'X-CSRF-Token': token,
      'Content-Type': 'application/json',
      'Cookie': latestCookies
    },
    agent: new https.Agent({ rejectUnauthorized: false })
  };

  const postData = JSON.stringify({
    url: 'https://github.tools.sap/I045696/abap-ai-test.git',
    branch: 'main'
  });

  const postReq = https.request(postOptions, (res) => {
    let body = '';
    res.on('data', chunk => body += chunk);
    res.on('end', () => {
      console.log('   Status:', res.statusCode);
      console.log('   Response:', body);
    });
  });
  postReq.on('error', console.error);
  postReq.write(postData);
  postReq.end();
}

pull();
