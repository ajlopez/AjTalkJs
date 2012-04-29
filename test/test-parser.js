
var ajtalk = require('../lib/ajtalk');
var Smalltalk = ajtalk.Smalltalk;

exports['Parse a name'] = function(test) {
	var lexer = new ajtalk.Lexer("name");
	var token = lexer.nextToken();

	test.notEqual(null, token);
	test.ok(token.isName());
	test.equal("name", token.value);
	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse a symbol'] = function(test) {
	var lexer = new ajtalk.Lexer("#Point");
	var token = lexer.nextToken();

	test.notEqual(null, token);
	test.ok(token.isSymbol());
	test.equal("Point", token.value);
	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse symbol and dot'] = function(test) {
	var lexer = new ajtalk.Lexer("#Point.");
	var token = lexer.nextToken();

	test.notEqual(null, token);
	test.ok(token.isSymbol());
	test.equal("Point", token.value);

	token = lexer.nextToken();

	test.notEqual(null, token);
	test.ok(token.isSeparator());
	test.equal(".", token.value);

	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse name and dot'] = function(test) {
	var lexer = new ajtalk.Lexer("name.");
	var token = lexer.nextToken();

	test.notEqual(null, token);
	test.ok(token.isName());
	test.equal("name", token.value);

	token = lexer.nextToken();

	test.notEqual(null, token);
	test.ok(token.isSeparator());
	test.equal(".", token.value);

	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Skip comment'] = function(test) {
	var lexer = new ajtalk.Lexer('"a comment"');
	var token = lexer.nextToken();
	
	test.equal(null, token);
	
	test.done();
}

exports['Parse name with comments'] = function(test) {
	var lexer = new ajtalk.Lexer('"first comment" name "second comment"');
	var token = lexer.nextToken();

	test.notEqual(null, token);
	test.ok(token.isName());
	test.equal("name", token.value);
	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse two names'] = function(test) {
	var lexer = new ajtalk.Lexer("self class");
	var token = lexer.nextToken();

	test.notEqual(null, token);
	test.ok(token.isName());
	test.equal("self", token.value);

	token = lexer.nextToken();

	test.notEqual(null, token);
	test.ok(token.isName());
	test.equal("class", token.value);

	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse integer number'] = function(test) {
	var lexer = new ajtalk.Lexer("123");
	var token = lexer.nextToken();

	test.notEqual(null, token);
	test.ok(token.isNumber());
	test.equal(123, token.value);

	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse integer number and dot'] = function(test) {
	var lexer = new ajtalk.Lexer("123. ");
	var token = lexer.nextToken();

	test.notEqual(null, token);
	test.ok(token.isNumber());
	test.equal(123, token.value);

	token = lexer.nextToken();

	test.notEqual(null, token);
	test.ok(token.isSeparator());
	test.equal('.', token.value);

	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse string'] = function(test) {
	var lexer = new ajtalk.Lexer("'foo'");
	var token = lexer.nextToken();

	test.notEqual(null, token);
	test.ok(token.isString());
	test.equal('foo', token.value);

	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse minus and name'] = function(test) {
	var lexer = new ajtalk.Lexer("-title");
	var token = lexer.nextToken();

	test.notEqual(null, token);
	test.ok(token.isOperator());
	test.equal('-', token.value);

	token = lexer.nextToken();

	test.notEqual(null, token);
	test.ok(token.isName());
	test.equal('title', token.value);

	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse keyword'] = function(test) {
	var lexer = new ajtalk.Lexer("at:");
	var token = lexer.nextToken();

	test.notEqual(null, token);
	test.equal('at:', token.value);
	test.ok(token.isKeyword());

	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse character'] = function(test) {
	var lexer = new ajtalk.Lexer("$(");
	var token = lexer.nextToken();

	test.notEqual(null, token);
	test.equal('(', token.value);
	test.ok(token.isCharacter());

	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse add operator'] = function(test) {
	var lexer = new ajtalk.Lexer('^');
	var token = lexer.nextToken();
	
	test.ok(token.isOperator());
	test.equal('^', token.value);
	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse return operator'] = function(test) {
	var lexer = new ajtalk.Lexer('^');
	var token = lexer.nextToken();
	
	test.ok(token.isOperator());
	test.equal('^', token.value);
	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse assignment operator'] = function(test) {
	var lexer = new ajtalk.Lexer(':=');
	var token = lexer.nextToken();
	
	test.ok(token.isOperator());
	test.equal(':=', token.value);
	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse less operator'] = function(test) {
	var lexer = new ajtalk.Lexer('<');
	var token = lexer.nextToken();

	test.ok(token.isOperator());
	test.equal('<', token.value);
	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse less or equal operator'] = function(test) {
	var lexer = new ajtalk.Lexer('<=');
	var token = lexer.nextToken();
	
	test.ok(token.isOperator());
	test.equal('<=', token.value);
	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse less or equal operator with spaces'] = function(test) {
	var lexer = new ajtalk.Lexer(' <= ');
	var token = lexer.nextToken();
	
	test.ok(token.isOperator());
	test.equal('<=', token.value);
	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse assignment'] = function(test) {
	var lexer = new ajtalk.Lexer('a := 3');
	var token = lexer.nextToken();
	
	test.ok(token.isName());
	test.equal('a', token.value);

	token = lexer.nextToken();
	test.ok(token.isOperator());
	test.equal(':=', token.value);

	token = lexer.nextToken();
	test.ok(token.isNumber());
	test.equal(3, token.value);

	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse return'] = function(test) {
	var lexer = new ajtalk.Lexer('^a');
	var token = lexer.nextToken();
	
	test.ok(token.isOperator());
	test.equal('^', token.value);

	token = lexer.nextToken();
	test.ok(token.isName());
	test.equal('a', token.value);

	test.equal(null, lexer.nextToken());
	
	test.done();
}

exports['Parse parameter name'] = function(test) {
	var lexer = new ajtalk.Lexer(':k');
	var token = lexer.nextToken();
	
	test.ok(token.isParameter());
	test.equal('k', token.value);

	test.equal(null, lexer.nextToken());
	
	test.done();
}

