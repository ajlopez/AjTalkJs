
var assert = require('assert');
var ajtalk = require('../lib/ajtalknew.js');


// Experimental

var obj = ajtalk.Smalltalk.Object.basicNew();

assert.equal(obj.klass, ajtalk.Smalltalk.Object);

var pointclass = ajtalk.Smalltalk.Object.defineSubclass('Point');

assert.ok(pointclass);

var point = pointclass.basicNew();

assert.ok(point);
assert.equal(point.klass, pointclass);

assert.ok(ajtalk.Smalltalk.Point);

ajtalk.Smalltalk.Object.defineMethod('add', function(x, y) { return x + y; });

assert.equal(3, obj.add(1, 2));
assert.equal(5, point.add(3, 2));

var compiler = new ajtalk.Compiler();

var method = compiler.compileMethod("add: x to: y ^x+y", ajtalk.Smalltalk.Object);
assert.equal("add:to:", method.name);
ajtalk.Smalltalk.Object.defineMethod(method.name, method);

assert.equal(3, obj.add_to_(1, 2));
assert.equal(5, point.add_to_(3, 2));

assert.equal(3, obj.sendMessage("add_to_", [1, 2]));
assert.equal(5, point.sendMessage("add_to_", [3, 2]));

