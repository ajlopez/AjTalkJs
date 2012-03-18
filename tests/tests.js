
var assert = require('assert');
var ajtalk = require('../lib/ajtalk.js');

var cls = new ajtalk.BaseClass();
var obj = new ajtalk.BaseObject(cls, 3);

assert.notEqual(null, obj);
assert.notEqual(null, obj.$klass);
assert.notEqual(null, obj.$variables);
assert.equal(3, obj.$variables.length);

var x = 0;

var method = function() {
	x = 1;
};

cls.defineMethod("setx", method);

assert.notEqual(null, obj.lookup("setx"));
assert.equal(method, obj.lookup("setx"));

assert.equal(0, x);
obj.sendMessage("setx", null);
assert.equal(1, x);


