
var lexer = require('../lib/lexer');

var TokenType = lexer.TokenType;

exports['get name'] = function (test) {
    var mylexer = lexer.createLexer("a");
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, "a");
    test.equal(token.type, TokenType.Name);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get two names'] = function (test) {
    var mylexer = lexer.createLexer("self class");
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, "self");
    test.equal(token.type, TokenType.Name);
    
    token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, "class");
    test.equal(token.type, TokenType.Name);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get name and punctuation'] = function (test) {
    var mylexer = lexer.createLexer("name)");
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, "name");
    test.equal(token.type, TokenType.Name);
    
    token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, ")");
    test.equal(token.type, TokenType.Punctuation);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get null when empty string'] = function (test) {
    var mylexer = lexer.createLexer("");
    
    test.equal(mylexer.nextToken(), null);
}

exports['get name with comments'] = function (test) {
    var mylexer = lexer.createLexer('"a comment" a "other comment"');
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, "a");
    test.equal(token.type, TokenType.Name);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get code in comment'] = function (test) {
    var mylexer = lexer.createLexer('"js:return 42;"');
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, "return 42;");
    test.equal(token.type, TokenType.Code);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get code in comment with double quotes'] = function (test) {
    var mylexer = lexer.createLexer('"js:return ""foo"";"');
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, 'return "foo";');
    test.equal(token.type, TokenType.Code);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get null when null string'] = function (test) {
    var mylexer = lexer.createLexer(null);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get name with spaces'] = function (test) {
    var mylexer = lexer.createLexer(" a  ");
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, "a");
    test.equal(token.type, TokenType.Name);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get keyword'] = function (test) {
    var mylexer = lexer.createLexer("with:");
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, "with:");
    test.equal(token.type, TokenType.Keyword);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get integer'] = function (test) {
    var mylexer = lexer.createLexer("123");
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, "123");
    test.equal(token.type, TokenType.Integer);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get integer and point'] = function (test) {
    var mylexer = lexer.createLexer("123.");
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, "123");
    test.equal(token.type, TokenType.Integer);
    
    token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, ".");
    test.equal(token.type, TokenType.Punctuation);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get symbol'] = function (test) {
    var mylexer = lexer.createLexer("#with:with:");
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, "with:with:");
    test.equal(token.type, TokenType.Symbol);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get symbol and dot'] = function(test) {
	var mylexer = lexer.createLexer("#Point.");
	var token = mylexer.nextToken();

	test.notEqual(null, token);
    test.equal(token.type, TokenType.Symbol);
	test.equal("Point", token.value);

	token = mylexer.nextToken();

	test.notEqual(null, token);
	test.equal(token.type, TokenType.Punctuation);
	test.equal(".", token.value);

	test.equal(null, mylexer.nextToken());
}

exports['get keyword and name'] = function (test) {
    var mylexer = lexer.createLexer("with:a");
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, "with:");
    test.equal(token.type, TokenType.Keyword);
    
    token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, "a");
    test.equal(token.type, TokenType.Name);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get punctuation marks'] = function (test) {
    var punctuations = "(),.|!{}[];";
    var mylexer = lexer.createLexer(punctuations);
    
    for (var k = 0; k < punctuations.length; k++) {    
        var token = mylexer.nextToken();
    
        test.ok(token);
        test.equal(token.value, punctuations[k]);
        test.equal(token.type, TokenType.Punctuation);
    }
}

exports['get begin dynamic array as punctuation'] = function (test) {
    var punctuations = "#(";
    var mylexer = lexer.createLexer(punctuations);
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, '#(');
    test.equal(token.type, TokenType.Punctuation);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get return empty dynamic array'] = function (test) {
    var punctuations = "^#()";
    var mylexer = lexer.createLexer(punctuations);
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, '^');
    test.equal(token.type, TokenType.Sign);
    
    token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, '#(');
    test.equal(token.type, TokenType.Punctuation);
    
    token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, ')');
    test.equal(token.type, TokenType.Punctuation);
    
    test.equal(mylexer.nextToken(), null);
}
exports['get string'] = function (test) {
    var mylexer = lexer.createLexer("'foo'");
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, "foo");
    test.equal(token.type, TokenType.String);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get unclosed string'] = function (test) {
    var mylexer = lexer.createLexer("'foo");
    
    test.throws(
        function() { mylexer.nextToken(); },
        function(err) {
            test.ok(err, 'unclosed string');
            return true;
        }
    );
}

exports['get string with quote'] = function(test) {
	var mylexer = lexer.createLexer("'foo\'\''");
	var token = mylexer.nextToken();

	test.notEqual(token, null);
	test.equal(token.type, TokenType.String);
	test.equal(token.value, 'foo\'');

	test.equal(mylexer.nextToken(), null);
}

exports['get real'] = function (test) {
    var mylexer = lexer.createLexer("123.45");
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, "123.45");
    test.equal(token.type, TokenType.Real);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get minus and name'] = function(test) {
	var mylexer = lexer.createLexer("-title");
	var token = mylexer.nextToken();

	test.notEqual(token, null);
	test.equal(token.type, TokenType.Sign);
	test.equal(token.value, '-');

	token = mylexer.nextToken();

	test.notEqual(token, null);
	test.equal(token.type, TokenType.Name);
	test.equal(token.value, 'title');

	test.equal(null, mylexer.nextToken());
}

exports['skip comment'] = function (test) {
    var mylexer = lexer.createLexer('"this is a comment"');
    
    test.equal(mylexer.nextToken(), null);
}

exports['skip multiline comment'] = function (test) {
    var mylexer = lexer.createLexer('"this is a\nmultiline\ncomment"');
    
    test.equal(mylexer.nextToken(), null);
}

exports['skip two comments'] = function (test) {
    var mylexer = lexer.createLexer('"this is a comment" "this is another comment"');
    
    test.equal(mylexer.nextToken(), null);
}

exports['get plus sign'] = function (test) {
    var mylexer = lexer.createLexer('+');
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, '+');
    test.equal(token.type, TokenType.Sign);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get number and plus sign'] = function (test) {
    var mylexer = lexer.createLexer('1+');
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, '1');
    test.equal(token.type, TokenType.Integer);
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, '+');
    test.equal(token.type, TokenType.Sign);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get name and plus sign'] = function (test) {
    var mylexer = lexer.createLexer('a+');
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, 'a');
    test.equal(token.type, TokenType.Name);
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, '+');
    test.equal(token.type, TokenType.Sign);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get return sign'] = function (test) {
    var mylexer = lexer.createLexer('^');
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, '^');
    test.equal(token.type, TokenType.Sign);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get assignment sign'] = function (test) {
    var mylexer = lexer.createLexer(':=');
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, ':=');
    test.equal(token.type, TokenType.Sign);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get right arrow'] = function (test) {
    var mylexer = lexer.createLexer('->');
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, '->');
    test.equal(token.type, TokenType.Sign);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get less'] = function (test) {
    var mylexer = lexer.createLexer('<');
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, '<');
    test.equal(token.type, TokenType.Sign);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get less or equal sign'] = function (test) {
    var mylexer = lexer.createLexer('<=');
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, '<=');
    test.equal(token.type, TokenType.Sign);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get character'] = function (test) {
    var mylexer = lexer.createLexer('$c');
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, 'c');
    test.equal(token.type, TokenType.Character);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get parameter'] = function (test) {
    var mylexer = lexer.createLexer(':a');
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, 'a');
    test.equal(token.type, TokenType.Parameter);
    
    test.equal(mylexer.nextToken(), null);
}

exports['get return name'] = function (test) {
    var mylexer = lexer.createLexer('^a');
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, '^');
    test.equal(token.type, TokenType.Sign);
    
    token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, 'a');
    test.equal(token.type, TokenType.Name);
    
    test.equal(mylexer.nextToken(), null);
}


