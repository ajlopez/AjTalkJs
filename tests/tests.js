
var assert = require('assert');
var ajtalk = require('../lib/ajtalk.js');

var cls = new ajtalk.BaseClass('SampleObject', ['a', 'b', 'c']);
var obj = new ajtalk.BaseObject(cls);

assert.notEqual(null, obj);
assert.notEqual(null, obj.klass);
assert.notEqual(null, obj.variables);
assert.equal(3, obj.variables.length);

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

var basicAt = function(position, value) {
	return this.variables[position];
};

var basicAtPut = function(position, value) {
	this.variables[position] = value;
};

cls.defineMethod("basicAt:", basicAt);
cls.defineMethod("basicAt:put:", basicAtPut);

obj.sendMessage("basicAt:put:", [0, 1]);
assert.equal(1, obj.variables[0]);
assert.equal(1, obj.sendMessage("basicAt:", [0]));

// Define method seta a := 10.

var method = new ajtalk.Block(0, 0);
method.values.push(10);
method.bytecodes = [ ajtalk.ByteCodes.GetValue, 0, ajtalk.ByteCodes.SetInstanceVariable, 0 ];

cls.defineMethod("seta", method);

obj.sendMessage("seta", null);

assert.equal(10, obj.variables[0]);

// Define method a: aValue a := aValue.

method = new ajtalk.Block(1, 0);
method.bytecodes = [ ajtalk.ByteCodes.GetArgument, 0, ajtalk.ByteCodes.SetInstanceVariable, 0 ];

cls.defineMethod("a:", method);
obj.sendMessage("a:", [20]);

assert.equal(20, obj.variables[0]);

// Get Value, Return in block

var block = new ajtalk.Block(0, 0);
block.values.push(3);
block.bytecodes = [ ajtalk.ByteCodes.GetValue, 0, ajtalk.ByteCodes.Return ];

assert.equal(3, block.apply(null, null));

// Add values to block

block = new ajtalk.Block(0, 0);
assert.equal(0, block.addValue(1));
assert.equal(1, block.addValue("a"));
assert.equal(2, block.addValue(null));
assert.equal(1, block.addValue("a"));
assert.equal(3, block.values.length);
assert.equal(1, block.values[0]);
assert.equal("a", block.values[1]);
assert.equal(null, block.values[2]);

// Arithmethic bycodes

block = new ajtalk.Block(2, 0);
block.bytecodes = [ ajtalk.ByteCodes.GetArgument, 0, 
			ajtalk.ByteCodes.GetArgument, 1, 
			ajtalk.ByteCodes.Add, 
			ajtalk.ByteCodes.Return ];
			
assert.equal(3, block.apply(null, [1, 2]));

block = new ajtalk.Block(2, 0);
block.bytecodes = [ ajtalk.ByteCodes.GetArgument, 0, 
			ajtalk.ByteCodes.GetArgument, 1, 
			ajtalk.ByteCodes.Subtract, 
			ajtalk.ByteCodes.Return ];
			
assert.equal(-1, block.apply(null, [1, 2]));

block = new ajtalk.Block(2, 0);
block.bytecodes = [ ajtalk.ByteCodes.GetArgument, 0, 
			ajtalk.ByteCodes.GetArgument, 1, 
			ajtalk.ByteCodes.Multiply, 
			ajtalk.ByteCodes.Return ];
			
assert.equal(6, block.apply(null, [3, 2]));

block = new ajtalk.Block(2, 0);
block.bytecodes = [ ajtalk.ByteCodes.GetArgument, 0, 
			ajtalk.ByteCodes.GetArgument, 1, 
			ajtalk.ByteCodes.Divide, 
			ajtalk.ByteCodes.Return ];
			
assert.equal(3/2, block.apply(null, [3, 2]));

// Set Global Variable

block = new ajtalk.Block(0, 0);
block.addValue('foo');
block.addValue('Bar');
block.bytecodes = [ ajtalk.ByteCodes.GetValue, 0, ajtalk.ByteCodes.SetGlobalVariable, 1 ];

block.apply();

assert.equal('foo', ajtalk.Smalltalk.Bar);

// Get Global Variable

block = new ajtalk.Block(0, 0);
block.addValue('Bar');
block.bytecodes = [ ajtalk.ByteCodes.GetGlobalVariable, 0, ajtalk.ByteCodes.Return ];

assert.equal('foo', ajtalk.Smalltalk.Bar);
assert.equal('foo', block.apply());

// Lexer

// Parse a name

var lexer = new ajtalk.Lexer("name");

var token = lexer.nextToken();

assert.notEqual(null, token);
assert.ok(token.isName());
assert.equal("name", token.value);
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

// Compiler

// Compile simple block

var compiler = new ajtalk.Compiler();

block = compiler.compileBlock('a');

assert.notEqual(null, block);

assert.equal(ajtalk.ByteCodes.GetGlobalVariable, block.bytecodes[0]);
assert.equal(0, block.bytecodes[1]);
assert.equal(1, block.values.length);
assert.equal("a", block.values[0]);

// Compile number

block = compiler.compileBlock('123');

assert.notEqual(null, block);

assert.equal(ajtalk.ByteCodes.GetValue, block.bytecodes[0]);
assert.equal(0, block.bytecodes[1]);
assert.equal(1, block.values.length);
assert.equal(123, block.values[0]);

// Compile string

block = compiler.compileBlock("'foo'");

assert.notEqual(null, block);

assert.equal(ajtalk.ByteCodes.GetValue, block.bytecodes[0]);
assert.equal(0, block.bytecodes[1]);
assert.equal(1, block.values.length);
assert.equal('foo', block.values[0]);

// Compile and Execute assignment

block = compiler.compileBlock("a := 3");

assert.notEqual(null, block);

assert.equal(ajtalk.ByteCodes.GetValue, block.bytecodes[0]);
assert.equal(1, block.bytecodes[1]);
assert.equal(2, block.values.length);
assert.equal(3, block.values[1]);
assert.equal(ajtalk.ByteCodes.SetGlobalVariable, block.bytecodes[2]);
assert.equal(0, block.bytecodes[3]);
assert.equal("a", block.values[0]);

block.apply();

assert.equal(3, ajtalk.Smalltalk.a);

// Compile and Execute return

block = compiler.compileBlock("^a");

assert.notEqual(null, block);

assert.equal(ajtalk.ByteCodes.GetGlobalVariable, block.bytecodes[0]);
assert.equal(0, block.bytecodes[1]);
assert.equal(1, block.values.length);
assert.equal("a", block.values[0]);

assert.equal(3, block.apply());

assert.equal(3, ajtalk.Smalltalk.a);

// Compile unary message

block = compiler.compileBlock("block value");

assert.notEqual(null, block);

assert.equal(ajtalk.ByteCodes.GetGlobalVariable, block.bytecodes[0]);
assert.equal(0, block.bytecodes[1]);
assert.equal(ajtalk.ByteCodes.GetValue, block.bytecodes[2]);
assert.equal(1, block.bytecodes[3]);
assert.equal(ajtalk.ByteCodes.SendMessage, block.bytecodes[4]);
assert.equal(0, block.bytecodes[5]);

// Compile And Execute binary message

block = compiler.compileBlock("1 + 3");

assert.notEqual(null, block);
assert.equal(2, block.values.length);
assert.equal(4, block.apply());

