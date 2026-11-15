function transportProfile(config, targetClient) {
  const client = String(targetClient ?? config.customizingClient ?? config.client);
  if (client.length !== 3 || !/^\d{3}$/.test(client)) throw new Error('Invalid customizing client');
  const logon = config.customizingLogon;
  if (client === String(config.client) && (logon == null || (targetClient != null && String(logon.client) !== client))) return config;
  if (!logon || Array.isArray(logon) || String(logon.client) !== client ||
      typeof logon.user !== 'string' || !logon.user.trim() || typeof logon.password !== 'string' || !logon.password.trim()) {
    throw new Error('Explicit customizingLogon client/user/password must match the customizing client');
  }
  // Never inherit a development-client task or credentials for a different logon.
  return { ...config, client, user: logon.user, password: logon.password, transport: logon.transport };
}

module.exports = { transportProfile };
