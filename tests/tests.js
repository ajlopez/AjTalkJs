
var assert = require('assert');
var ajtalk = require('../lib/ajtalk.js');

var cls = new ajtalk.BaseClass('SampleObject', ['a', 'b', 'c']);
var obj = new ajtalk.BaseObject(cls);

assert.notEqual(null, obj);
assert.notEqual(null, obj.$klass);
assert.notEqual(null, obj.$variables);
assert.equal(3, obj.$variables.length);

var x = 0;

var setx = function() {
	x = 1;
};

cls.defineMethod("setx", setx);

assert.notEqual(null, obj.lookup("setx"));
assert.equal(setx, obj.lookup("setx"));

assert.equal(0, x);
obj.sendMessage("setx", null);
assert.equal(1, x);

var setxtovalue = function(value) {
	x = value;
};

cls.defineMethod("setx:", setxtovalue);

obj.sendMessage("setx:", [2]);
assert.equal(2, x);
