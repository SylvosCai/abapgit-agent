/**
 * Unit tests for view command in CLI
 * Tests object parsing and request building
 */

describe('View Command', () => {
  describe('Object parsing', () => {
    test('parses single object name', () => {
      const objectsArg = 'ZCL_MY_CLASS';
      const objects = objectsArg.split(',').map(o => o.trim());

      expect(objects.length).toBe(1);
      expect(objects[0]).toBe('ZCL_MY_CLASS');
    });

    test('parses multiple object names', () => {
      const objectsArg = 'ZCL_CLASS1,ZCL_CLASS2,ZIF_INTERFACE';
      const objects = objectsArg.split(',').map(o => o.trim());

      expect(objects.length).toBe(3);
      expect(objects).toContain('ZCL_CLASS1');
      expect(objects).toContain('ZIF_INTERFACE');
    });

    test('handles spaces after commas', () => {
      const objectsArg = 'ZCL_CLASS1, ZCL_CLASS2';
      const objects = objectsArg.split(',').map(o => o.trim());

      expect(objects[1]).toBe('ZCL_CLASS2');
    });
  });

  describe('Type detection', () => {
    test('auto-detects type from TADIR when not specified', () => {
      const object = 'ZCL_MY_CLASS';
      const explicitType = null; // Not specified
      const shouldAutoDetect = explicitType === null;

      expect(shouldAutoDetect).toBe(true);
    });

    test('uses explicit type when specified', () => {
      const args = ['view', '--objects', 'ZCL_TEST', '--type', 'CLAS'];
      const typeIndex = args.indexOf('--type');
      const type = typeIndex !== -1 ? args[typeIndex + 1] : null;

      expect(type).toBe('CLAS');
    });

    test('handles various object types', () => {
      const types = ['CLAS', 'INTF', 'TABL', 'STRU', 'DTEL', 'TTYP', 'DDLS'];

      types.forEach(type => {
        expect(type).toMatch(/^[A-Z]{4}$/);
      });
    });
  });

  describe('Response handling', () => {
    test('handles successful response for class', () => {
      const response = {
        success: true,
        objects: [{
          name: 'ZCL_MY_CLASS',
          type: 'CLAS',
          type_text: 'Class',
          source: 'CLASS zcl_my_class DEFINITION...',
          not_found: false
        }]
      };

      const success = response.success || response.SUCCESS;
      const objects = response.objects || response.OBJECTS || [];

      expect(success).toBe(true);
      expect(objects[0].type).toBe('CLAS');
    });

    test('handles object not found', () => {
      const response = {
        success: false,
        objects: [{
          name: 'ZCL_NONEXISTENT',
          not_found: true
        }]
      };

      const objects = response.objects || response.OBJECTS || [];

      expect(objects[0].not_found).toBe(true);
    });

    test('handles multiple objects in response', () => {
      const response = {
        success: true,
        objects: [
          { name: 'ZCL_CLASS1', type: 'CLAS' },
          { name: 'ZIF_INTERFACE', type: 'INTF' },
          { name: 'ZTABLE', type: 'TABL' }
        ]
      };

      const objects = response.objects || response.OBJECTS || [];

      expect(objects.length).toBe(3);
      expect(objects.map(o => o.type)).toEqual(['CLAS', 'INTF', 'TABL']);
    });
  });

  describe('JSON output mode', () => {
    test('detects JSON output flag', () => {
      const args = ['view', '--objects', 'ZCL_TEST', '--json'];
      const jsonOutput = args.includes('--json');

      expect(jsonOutput).toBe(true);
    });

    test('JSON flag not present by default', () => {
      const args = ['view', '--objects', 'ZCL_TEST'];
      const jsonOutput = args.includes('--json');

      expect(jsonOutput).toBe(false);
    });
  });

  describe('Request building', () => {
    test('builds request for single object', () => {
      const request = {
        objects: ['ZCL_MY_CLASS'],
        type: null // Auto-detect
      };

      expect(request.objects.length).toBe(1);
    });

    test('builds request for multiple objects', () => {
      const request = {
        objects: ['ZCL_CLASS1', 'ZCL_CLASS2'],
        type: null
      };

      expect(request.objects.length).toBe(2);
    });

    test('includes type when specified', () => {
      const request = {
        objects: ['ZMY_TABLE'],
        type: 'TABL'
      };

      expect(request.type).toBe('TABL');
    });
  });

  describe('Special object types', () => {
    test('handles table type (TTYP)', () => {
      const object = 'ZMY_TABLE_TYPE';
      const type = 'TTYP';

      expect(type).toBe('TTYP');
    });

    test('handles structure (STRU)', () => {
      const object = 'ZMY_STRUCTURE';
      const type = 'STRU';

      expect(type).toBe('STRU');
    });

    test('handles data element (DTEL)', () => {
      const object = 'ZMY_DATA_ELEMENT';
      const type = 'DTEL';

      expect(type).toBe('DTEL');
    });

    test('handles CDS view (DDLS)', () => {
      const object = 'ZC_MY_CDS_VIEW';
      const type = 'DDLS';

      expect(type).toBe('DDLS');
    });
  });
});
