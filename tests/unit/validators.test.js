/**
 * Unit tests for validators
 * Tests input validation utilities
 */

const validators = require('../../src/utils/validators');

describe('validators', () => {
  describe('convertDatesInWhereClause', () => {
    test('converts ISO date format to ABAP DATS format', () => {
      const input = "FLDATE = '2024-01-15'";
      const expected = "FLDATE = '20240115'";
      expect(validators.convertDatesInWhereClause(input)).toBe(expected);
    });

    test('converts multiple dates in WHERE clause', () => {
      const input = "FLDATE >= '2024-01-15' AND FLDATE <= '2024-12-31'";
      const expected = "FLDATE >= '20240115' AND FLDATE <= '20241231'";
      expect(validators.convertDatesInWhereClause(input)).toBe(expected);
    });

    test('leaves non-date values unchanged', () => {
      const input = "CARRID = 'AA' AND CONNID = '0017'";
      expect(validators.convertDatesInWhereClause(input)).toBe(input);
    });

    test('returns unchanged when WHERE clause is null', () => {
      expect(validators.convertDatesInWhereClause(null)).toBeNull();
    });

    test('returns unchanged when WHERE clause is undefined', () => {
      expect(validators.convertDatesInWhereClause(undefined)).toBeUndefined();
    });

    test('handles dates with different separators', () => {
      const input = "FLDATE = '2024-01-15'";
      const expected = "FLDATE = '20240115'";
      expect(validators.convertDatesInWhereClause(input)).toBe(expected);
    });

    test('preserves dates already in ABAP format', () => {
      const input = "FLDATE = '20240115'";
      expect(validators.convertDatesInWhereClause(input)).toBe(input);
    });
  });

  describe('isValidPackageName', () => {
    test('accepts package starting with $', () => {
      expect(validators.isValidPackageName('$MY_PACKAGE')).toBe(true);
    });

    test('accepts package starting with Z', () => {
      expect(validators.isValidPackageName('ZTEST_PACKAGE')).toBe(true);
    });

    test('accepts package starting with Y', () => {
      expect(validators.isValidPackageName('YTEST_PACKAGE')).toBe(true);
    });

    test('accepts package with underscores', () => {
      expect(validators.isValidPackageName('$MY_TEST_PACKAGE')).toBe(true);
    });

    test('accepts package with numbers', () => {
      expect(validators.isValidPackageName('$PACKAGE123')).toBe(true);
    });

    test('accepts lowercase package name', () => {
      expect(validators.isValidPackageName('$my_package')).toBe(true);
    });

    test('rejects package with spaces', () => {
      expect(validators.isValidPackageName('ZINVALID PACKAGE')).toBe(false);
    });

    test('rejects package starting with invalid character', () => {
      expect(validators.isValidPackageName('AINVALID')).toBe(false);
    });

    test('rejects empty package name', () => {
      expect(validators.isValidPackageName('')).toBe(false);
    });

    test('rejects null package name', () => {
      expect(validators.isValidPackageName(null)).toBe(false);
    });

    test('rejects undefined package name', () => {
      expect(validators.isValidPackageName(undefined)).toBe(false);
    });

    test('rejects package name longer than 30 characters', () => {
      expect(validators.isValidPackageName('$' + 'A'.repeat(30))).toBe(false);
    });

    test('accepts package name exactly 30 characters', () => {
      expect(validators.isValidPackageName('$' + 'A'.repeat(29))).toBe(true);
    });

    test('rejects package with special characters', () => {
      expect(validators.isValidPackageName('$MY-PACKAGE')).toBe(false);
    });
  });

  describe('isValidObjectName', () => {
    test('accepts valid class name', () => {
      expect(validators.isValidObjectName('ZCL_MY_CLASS')).toBe(true);
    });

    test('accepts valid interface name', () => {
      expect(validators.isValidObjectName('ZIF_MY_INTERFACE')).toBe(true);
    });

    test('accepts name with underscores', () => {
      expect(validators.isValidObjectName('ZMY_TEST_OBJECT')).toBe(true);
    });

    test('accepts name with numbers', () => {
      expect(validators.isValidObjectName('ZOBJ123')).toBe(true);
    });

    test('accepts name with forward slash', () => {
      expect(validators.isValidObjectName('Z/NAMESPACE/CLASS')).toBe(true);
    });

    test('accepts lowercase object name', () => {
      expect(validators.isValidObjectName('zcl_my_class')).toBe(true);
    });

    test('rejects empty object name', () => {
      expect(validators.isValidObjectName('')).toBe(false);
    });

    test('rejects null object name', () => {
      expect(validators.isValidObjectName(null)).toBe(false);
    });

    test('rejects undefined object name', () => {
      expect(validators.isValidObjectName(undefined)).toBe(false);
    });

    test('rejects object name with spaces', () => {
      expect(validators.isValidObjectName('ZCL MY CLASS')).toBe(false);
    });

    test('rejects object name longer than 30 characters', () => {
      expect(validators.isValidObjectName('Z' + 'A'.repeat(30))).toBe(false);
    });

    test('accepts object name exactly 30 characters', () => {
      expect(validators.isValidObjectName('Z' + 'A'.repeat(29))).toBe(true);
    });
  });

  describe('parseObjectFromFile', () => {
    test('parses class file correctly', () => {
      const result = validators.parseObjectFromFile('src/zcl_my_class.clas.abap');
      expect(result).toEqual({ name: 'ZCL_MY_CLASS', type: 'CLAS' });
    });

    test('parses interface file correctly', () => {
      const result = validators.parseObjectFromFile('src/zif_my_interface.intf.abap');
      expect(result).toEqual({ name: 'ZIF_MY_INTERFACE', type: 'INTF' });
    });

    test('parses program file correctly', () => {
      const result = validators.parseObjectFromFile('src/zmy_program.prog.abap');
      expect(result).toEqual({ name: 'ZMY_PROGRAM', type: 'PROG' });
    });

    test('parses function group file correctly', () => {
      const result = validators.parseObjectFromFile('src/zmy_fugr.fugr.xml');
      expect(result).toEqual({ name: 'ZMY_FUGR', type: 'FUGR' });
    });

    test('parses CDS view file correctly', () => {
      const result = validators.parseObjectFromFile('src/zc_my_view.ddls.asddls');
      expect(result).toEqual({ name: 'ZC_MY_VIEW', type: 'DDLS' });
    });

    test('parses table file correctly', () => {
      const result = validators.parseObjectFromFile('src/zmy_table.tabl.xml');
      expect(result).toEqual({ name: 'ZMY_TABLE', type: 'TABL' });
    });

    test('parses data element file correctly', () => {
      const result = validators.parseObjectFromFile('src/zmy_dtel.dtel.xml');
      expect(result).toEqual({ name: 'ZMY_DTEL', type: 'DTEL' });
    });

    test('parses table type file correctly', () => {
      const result = validators.parseObjectFromFile('src/zmy_ttyp.ttyp.xml');
      expect(result).toEqual({ name: 'ZMY_TTYP', type: 'TTYP' });
    });

    test('parses structure file correctly', () => {
      const result = validators.parseObjectFromFile('src/zmy_struct.stru.xml');
      expect(result).toEqual({ name: 'ZMY_STRUCT', type: 'STRU' });
    });

    test('handles file path with subdirectories', () => {
      const result = validators.parseObjectFromFile('src/subfolder/zcl_test.clas.abap');
      expect(result).toEqual({ name: 'ZCL_TEST', type: 'CLAS' });
    });

    test('converts lowercase to uppercase', () => {
      const result = validators.parseObjectFromFile('zcl_my_class.clas.abap');
      expect(result).toEqual({ name: 'ZCL_MY_CLASS', type: 'CLAS' });
    });

    test('returns null for invalid file format', () => {
      const result = validators.parseObjectFromFile('invalid_file.txt');
      expect(result).toBeNull();
    });

    test('returns null for file without extension', () => {
      const result = validators.parseObjectFromFile('zcl_my_class');
      expect(result).toBeNull();
    });

    test('handles class locals file', () => {
      const result = validators.parseObjectFromFile('zcl_my_class.clas.locals_def.abap');
      expect(result).toEqual({ name: 'ZCL_MY_CLASS', type: 'CLAS' });
    });
  });
});
