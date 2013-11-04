"use strict";

describe('returnless function behavior', function () {
    it('should be defined', function () {
	var a = function () {};
	expect(a()).toBe(undefined);
    });
});
