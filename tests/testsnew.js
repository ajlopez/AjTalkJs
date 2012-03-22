
var assert = require('assert');
var ajtalk = require('../lib/ajtalknew.js');
var Smalltalk = ajtalk.Smalltalk;

// New Experimental Implementation tests

// native basicNew
var obj = Smalltalk.Object.basicNew();

assert.equal(obj.klass, Smalltalk.Object);

// compiled new
var obj2 = Smalltalk.Object.new();

assert.equal(obj2.klass, Smalltalk.Object);

var pointclass = Smalltalk.Object.defineSubclass('Point', ['x', 'y']);

assert.ok(pointclass);
assert.ok(Smalltalk.Point);
assert.equal(pointclass, Smalltalk.Point);
assert.ok(Smalltalk.Point.super);
assert.equal(Smalltalk.Point.super, Smalltalk.Object);

assert.ok(pointclass.instvarnames);
assert.equal(2, pointclass.instvarnames.length);
assert.equal('x', pointclass.instvarnames[0]);
assert.equal('y', pointclass.instvarnames[1]);

var point = pointclass.basicNew();

assert.ok(point);
assert.equal(point.klass, pointclass);

var point2 = pointclass.new();

assert.ok(point2);
assert.equal(point2.klass, pointclass);

Smalltalk.Object.defineMethod('add', function(x, y) { return x + y; });

assert.equal(3, obj.add(1, 2));
assert.equal(5, point.add(3, 2));

var compiler = new ajtalk.Compiler();

var method = compiler.compileMethod("add: x to: y ^x+y", Smalltalk.Object);
assert.equal("add:to:", method.name);
Smalltalk.Object.defineMethod(method.name, method);

assert.equal(3, obj.add_to_(1, 2));
assert.equal(5, point.add_to_(3, 2));

assert.equal(3, obj.sendMessage("add_to_", [1, 2]));
assert.equal(5, point.sendMessage("add_to_", [3, 2]));

Smalltalk.Object.compileMethod_("add1: x ^ x + 1.");
assert.equal(3, obj.add1_(2));

Smalltalk.Point.compileMethod_("x: aValue x := aValue.");
assert.ok(Smalltalk.Point.func.prototype.x_);

point.sendMessage("x_", [10]);
assert.equal(10, point.$x);

Smalltalk.Point.compileMethod_("x ^x");
assert.ok(Smalltalk.Point.func.prototype.x);
assert.equal(10, point.x());

