
var assert = require('assert');
var ajtalk = require('../lib/ajtalk.js');
var Smalltalk = ajtalk.Smalltalk;

// Lexer

// Parse a name

var lexer = new ajtalk.Lexer("name");

var token = lexer.nextToken();

assert.notEqual(null, token);
assert.ok(token.isName());
assert.equal("name", token.value);
assert.equal(null, lexer.nextToken());

// Parse a name and dot

var lexer = new ajtalk.Lexer("name.");

var token = lexer.nextToken();

assert.notEqual(null, token);
assert.ok(token.isName());
assert.equal("name", token.value);

token = lexer.nextToken();

assert.notEqual(null, token);
assert.ok(token.isSeparator());
assert.equal(".", token.value);

assert.equal(null, lexer.nextToken());

// Skip comment

lexer = new ajtalk.Lexer('"a comment"');
token = lexer.nextToken();
assert.equal(null, token);

// Parse a name with comments

lexer = new ajtalk.Lexer('"first comment" name "second comment"');

token = lexer.nextToken();

assert.notEqual(null, token);
assert.ok(token.isName());
assert.equal("name", token.value);
assert.equal(null, lexer.nextToken());

// Parse two names

lexer = new ajtalk.Lexer("self class");

token = lexer.nextToken();

assert.notEqual(null, token);
assert.ok(token.isName());
assert.equal("self", token.value);

token = lexer.nextToken();

assert.notEqual(null, token);
assert.ok(token.isName());
assert.equal("class", token.value);

assert.equal(null, lexer.nextToken());

// Parse an integer number

lexer = new ajtalk.Lexer("123");

token = lexer.nextToken();

assert.notEqual(null, token);
assert.ok(token.isNumber());
assert.equal(123, token.value);

assert.equal(null, lexer.nextToken());

// Parse an integer number and dot

lexer = new ajtalk.Lexer("123. ");

token = lexer.nextToken();

assert.notEqual(null, token);
assert.ok(token.isNumber());
assert.equal(123, token.value);

token = lexer.nextToken();

assert.notEqual(null, token);
assert.ok(token.isSeparator());
assert.equal('.', token.value);

assert.equal(null, lexer.nextToken());

// Parse a string

lexer = new ajtalk.Lexer("'foo'");

token = lexer.nextToken();

assert.notEqual(null, token);
assert.ok(token.isString());
assert.equal('foo', token.value);

assert.equal(null, lexer.nextToken());

// Parse a keyword

lexer = new ajtalk.Lexer("at:");

token = lexer.nextToken();

assert.notEqual(null, token);
assert.equal('at:', token.value);
assert.ok(token.isKeyword());

assert.equal(null, lexer.nextToken());

// Parse add operator

lexer = new ajtalk.Lexer('+');
token = lexer.nextToken();
assert.ok(token.isOperator());
assert.equal('+', token.value);
assert.equal(null, lexer.nextToken());

// Parse return operator

lexer = new ajtalk.Lexer('^');
token = lexer.nextToken();
assert.ok(token.isOperator());
assert.equal('^', token.value);
assert.equal(null, lexer.nextToken());

// Parse assignment operator

lexer = new ajtalk.Lexer(':=');
token = lexer.nextToken();
assert.ok(token.isOperator());
assert.equal(':=', token.value);
assert.equal(null, lexer.nextToken());

// Parse assignment

lexer = new ajtalk.Lexer('a := 3');

token = lexer.nextToken();
assert.ok(token.isName());
assert.equal('a', token.value);

token = lexer.nextToken();
assert.ok(token.isOperator());
assert.equal(':=', token.value);

token = lexer.nextToken();
assert.ok(token.isNumber());
assert.equal(3, token.value);

assert.equal(null, lexer.nextToken());

// Parse return

lexer = new ajtalk.Lexer('^a');

token = lexer.nextToken();
assert.ok(token.isOperator());
assert.equal('^', token.value);

token = lexer.nextToken();
assert.ok(token.isName());
assert.equal('a', token.value);

assert.equal(null, lexer.nextToken());


// New Experimental Implementation tests

// Objects methods

var p = {};

p.$age = 800;

assert.equal(800, p.nat_('$age'));
assert.equal('Adam', p.nat_put_('$name', 'Adam'));
assert.equal('Adam', p.nat_('$name'));

var v = [1,2,3];

assert.equal(3, v.nat_('length'));
assert.equal(2, v.nat_(1));
v.napply_with_('push', [4]);
assert.equal(4, v.nat_('length'));
assert.equal(4, v.nat_(3));

assert.equal(4, v.napply_('pop'));
assert.equal(3, v.length);

var q = Object.nnew();

assert.notEqual(null, q);

var v2 = Array.nnew_([7]);

assert.equal(7, v2.length);
assert.equal(7, v2.nat_('length'));
assert.ok(v2 instanceof Array);

var n = Number.nnew_([4]);
assert.equal('4', n.sendMessage('toString'));

assert.ok(Smalltalk.nat_('Global'));
assert.ok(Smalltalk.nat_('Global').nat_('Number'));

// Object

assert.equal(null, Smalltalk.Object.instvarnames);
assert.equal(null, Smalltalk.Object.clsvarnames);

// basicNew

var obj = Smalltalk.Object.basicNew();

assert.equal(obj.klass, Smalltalk.Object);
assert.ok(Smalltalk.Object.func);
assert.ok(Smalltalk.Object.klass);
assert.ok(Smalltalk.Object.klass.func);

// class method

assert.ok(obj.class());
assert.equal(Smalltalk.Object, obj.class());

// name method in class

assert.ok(Smalltalk.Object.proto);
assert.ok(Smalltalk.Object.proto.name);
assert.equal('Object', Smalltalk.Object.name());

// name method in metaclass

assert.equal('Object class', Smalltalk.Object.class().name());

// class method in class

assert.ok(obj.class().class());

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

var compiler = new ajtalk.Compiler();
var block = compiler.compileBlock("Object compileMethod: 'one ^1'");
var result = block.apply();

assert.ok(Smalltalk.Object.func.prototype.one);
assert.ok(typeof Smalltalk.Object.func.prototype.one == "function");

assert.equal(1, obj.one());

block = compiler.compileBlock("Object nat: '$name'");
result = block.apply();

assert.equal('Object', result);

// Subclass

block = compiler.compileBlock("Object subclass: 'Point' instanceVariableNames: 'x y' classVariableNames: ''");
result = block.apply();

assert.ok(Smalltalk.Point);
assert.ok(Smalltalk.Point.instvarnames);
assert.equal(null, Smalltalk.Point.clsvarnames);
assert.equal(2, Smalltalk.Point.instvarnames.length);
assert.equal('x', Smalltalk.Point.instvarnames[0]);
assert.equal('y', Smalltalk.Point.instvarnames[1]);

// Compile methods

block = compiler.compileBlock("Point compileMethod: 'x: aValue x := aValue'");
block.apply();

block = compiler.compileBlock("Point compileMethod: 'x ^x");
block.apply();

block = compiler.compileBlock("Point new");
var point = block.apply();
point.x_(10);
assert.ok(10, point.x());

// Compiler

block = compiler.compileBlock("{ 1. 2. 3+5. Global }");
result = block.apply();

assert.ok(result);
assert.equal(4, result.length);
assert.equal(1, result[0]);
assert.equal(2, result[1]);
assert.equal(8, result[2]);
assert.equal(Smalltalk.Global, result[3]);

// Chunck Reader

var chreader = new ajtalk.ChunckReader(null);
assert.equal(null, chreader.nextChunck());

chreader = new ajtalk.ChunckReader('a:=1');
assert.equal('a:=1', chreader.nextChunck());
assert.equal(null, chreader.nextChunck());

chreader = new ajtalk.ChunckReader('a:=1! b:=1! !');
assert.equal('a:=1', chreader.nextChunck());
assert.equal(' b:=1', chreader.nextChunck());
assert.equal(' ', chreader.nextChunck());
assert.equal(null, chreader.nextChunck());

chreader = new ajtalk.ChunckReader("self error: 'Error!!'! b:=1! !");
assert.equal("self error: 'Error!'", chreader.nextChunck());
assert.equal(' b:=1', chreader.nextChunck());
assert.equal(' ', chreader.nextChunck());
assert.equal(null, chreader.nextChunck());

chreader = new ajtalk.ChunckReader("self error: 'Error!!!!'! b:=1! !");
assert.equal("self error: 'Error!!'", chreader.nextChunck());
assert.equal(' b:=1', chreader.nextChunck());
assert.equal(' ', chreader.nextChunck());
assert.equal(null, chreader.nextChunck());

chreader = new ajtalk.ChunckReader("self error: 'Error!!!!'");
assert.equal("self error: 'Error!!'", chreader.nextChunck());
assert.equal(null, chreader.nextChunck());
