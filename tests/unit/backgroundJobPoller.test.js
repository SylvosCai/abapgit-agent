/**
 * Unit tests for backgroundJobPoller utility
 */

const {
  formatTimestamp,
  calculateTimeSpent,
  parseAbapTimestamp,
  displayProgress
} = require('../../src/utils/backgroundJobPoller');

describe('backgroundJobPoller', () => {
  describe('formatTimestamp', () => {
    it('should format ABAP timestamp to readable format', () => {
      // 2026-03-05 10:30:45 UTC
      const timestamp = '20260305103045';
      const result = formatTimestamp(timestamp);

      // Result should be in local timezone
      expect(result).toMatch(/^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}$/);
    });

    it('should return original value for invalid timestamp', () => {
      expect(formatTimestamp('invalid')).toBe('invalid');
      expect(formatTimestamp('12345')).toBe('12345');
      expect(formatTimestamp(null)).toBe(null);
      expect(formatTimestamp(undefined)).toBe(undefined);
    });
  });

  describe('parseAbapTimestamp', () => {
    it('should parse ABAP timestamp to Date object', () => {
      const timestamp = '20260305103045';
      const result = parseAbapTimestamp(timestamp);

      expect(result).toBeInstanceOf(Date);
      expect(result.getUTCFullYear()).toBe(2026);
      expect(result.getUTCMonth()).toBe(2); // March (0-indexed)
      expect(result.getUTCDate()).toBe(5);
      expect(result.getUTCHours()).toBe(10);
      expect(result.getUTCMinutes()).toBe(30);
      expect(result.getUTCSeconds()).toBe(45);
    });

    it('should return null for invalid timestamp', () => {
      expect(parseAbapTimestamp('invalid')).toBeNull();
      expect(parseAbapTimestamp('12345')).toBeNull();
      expect(parseAbapTimestamp(null)).toBeNull();
      expect(parseAbapTimestamp(undefined)).toBeNull();
    });
  });

  describe('calculateTimeSpent', () => {
    it('should calculate seconds', () => {
      const start = '20260305103000'; // 10:30:00
      const end = '20260305103015';   // 10:30:15

      expect(calculateTimeSpent(start, end)).toBe('15 seconds');
    });

    it('should calculate minutes', () => {
      const start = '20260305103000'; // 10:30:00
      const end = '20260305103200';   // 10:32:00

      expect(calculateTimeSpent(start, end)).toBe('2 minutes');
    });

    it('should calculate minutes and seconds', () => {
      const start = '20260305103000'; // 10:30:00
      const end = '20260305103145';   // 10:31:45

      expect(calculateTimeSpent(start, end)).toBe('1 minute 45 seconds');
    });

    it('should calculate hours and minutes', () => {
      const start = '20260305103000'; // 10:30:00
      const end = '20260305123015';   // 12:30:15

      expect(calculateTimeSpent(start, end)).toBe('2 hours 0 minutes');
    });

    it('should handle single second', () => {
      const start = '20260305103000';
      const end = '20260305103001';

      expect(calculateTimeSpent(start, end)).toBe('1 second');
    });

    it('should return unknown for invalid timestamps', () => {
      expect(calculateTimeSpent('invalid', '20260305103000')).toBe('unknown');
      expect(calculateTimeSpent('20260305103000', 'invalid')).toBe('unknown');
      expect(calculateTimeSpent(null, null)).toBe('unknown');
    });
  });

  describe('displayProgress', () => {
    let originalStdoutWrite;

    beforeEach(() => {
      originalStdoutWrite = process.stdout.write;
      process.stdout.write = jest.fn();
    });

    afterEach(() => {
      process.stdout.write = originalStdoutWrite;
    });

    it('should display progress bar with message', () => {
      displayProgress(50, 'Processing...');

      expect(process.stdout.write).toHaveBeenCalledWith(
        expect.stringContaining('[===============               ]')
      );
      expect(process.stdout.write).toHaveBeenCalledWith(
        expect.stringContaining('Processing...')
      );
    });

    it('should display progress bar with percentage when showPercentage=true', () => {
      displayProgress(75, 'Almost done', { showPercentage: true });

      expect(process.stdout.write).toHaveBeenCalledWith(
        expect.stringContaining('75%')
      );
    });

    it('should handle 0% progress', () => {
      displayProgress(0, 'Starting...');

      expect(process.stdout.write).toHaveBeenCalledWith(
        expect.stringContaining('[                              ]')
      );
    });

    it('should handle 100% progress', () => {
      displayProgress(100, 'Complete');

      expect(process.stdout.write).toHaveBeenCalledWith(
        expect.stringContaining('[==============================]')
      );
    });
  });
});
