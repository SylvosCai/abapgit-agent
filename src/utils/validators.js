/**
 * Input validation utilities
 */

/**
 * Convert ISO date formats (YYYY-MM-DD) to ABAP DATS format (YYYYMMDD) in WHERE clause
 * This allows users to use familiar ISO date formats while ensuring compatibility with ABAP SQL
 * @param {string} whereClause - SQL WHERE clause
 * @returns {string} - WHERE clause with dates converted to YYYYMMDD format
 */
function convertDatesInWhereClause(whereClause) {
  if (!whereClause) return whereClause;

  // Pattern to match ISO date format: 'YYYY-MM-DD'
  const isoDatePattern = /'\d{4}-\d{2}-\d{2}'/g;

  return whereClause.replace(isoDatePattern, (match) => {
    // Extract YYYY, MM, DD from 'YYYY-MM-DD'
    const dateContent = match.slice(1, -1); // Remove quotes: YYYY-MM-DD
    const [year, month, day] = dateContent.split('-');
    // Return in ABAP format: 'YYYYMMDD'
    return `'${year}${month}${day}'`;
  });
}

/**
 * Validate package name format
 * @param {string} packageName - ABAP package name
 * @returns {boolean} True if valid
 */
function isValidPackageName(packageName) {
  if (!packageName) return false;
  // Package names: $PACKAGE or ZPACKAGE (can contain _ and alphanumeric)
  return /^(\$|Z|Y)[A-Z0-9_]{0,29}$/i.test(packageName);
}

/**
 * Validate object name format
 * @param {string} objectName - ABAP object name
 * @returns {boolean} True if valid
 */
function isValidObjectName(objectName) {
  if (!objectName) return false;
  // Object names: up to 30 characters, alphanumeric + underscore
  return /^[A-Z0-9_/]{1,30}$/i.test(objectName);
}

/**
 * Parse file path to extract object type and name
 * @param {string} filePath - File path (e.g., "src/zcl_my_class.clas.abap")
 * @returns {object|null} {type, name} or null if invalid
 */
function parseObjectFromFile(filePath) {
  const fileName = filePath.split('/').pop();

  // Match patterns: zcl_class.clas.abap, zif_intf.intf.abap, etc.
  const match = fileName.match(/^([a-z0-9_]+)\.(clas|intf|prog|fugr|ddls|tabl|dtel|ttyp|stru)\..*$/i);

  if (!match) return null;

  return {
    name: match[1].toUpperCase(),
    type: match[2].toUpperCase()
  };
}

module.exports = {
  convertDatesInWhereClause,
  isValidPackageName,
  isValidObjectName,
  parseObjectFromFile
};
