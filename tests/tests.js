
var assert = require('assert');
var ajtalk = require('../lib/ajtalk.js');

var cls = new ajtalk.BaseClass();
var obj = new ajtalk.BaseObject(cls, 3);

assert.notEqual(null, obj);
assert.notEqual(null, obj.$klass);
assert.notEqual(null, obj.$variables);
assert.equal(3, obj.$variables.length);

