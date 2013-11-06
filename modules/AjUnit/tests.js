
var ajtalk = require('../..'),
    assert = require('assert');

ajtalk.load('Init.st');

assert.ok(ajtalk.Smalltalk.AjUnit);

ajtalk.load('AjUnit-Tests.st');

assert.ok(ajtalk.Smalltalk.AssertTests);

ajtalk.execute("AssertTests run");

