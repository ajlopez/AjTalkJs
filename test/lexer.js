
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

exports['get null when empty string'] = function (test) {
    var mylexer = lexer.createLexer("");
    
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

exports['get symbol'] = function (test) {
    var mylexer = lexer.createLexer("#with:with:");
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, "with:with:");
    test.equal(token.type, TokenType.Symbol);
    
    test.equal(mylexer.nextToken(), null);
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
    var punctuations = "(),.|!";
    var mylexer = lexer.createLexer(punctuations);
    
    for (var k = 0; k < punctuations.length; k++) {    
        var token = mylexer.nextToken();
    
        test.ok(token);
        test.equal(token.value, punctuations[k]);
        test.equal(token.type, TokenType.Punctuation);
    }
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

exports['get real'] = function (test) {
    var mylexer = lexer.createLexer("123.45");
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, "123.45");
    test.equal(token.type, TokenType.Real);
    
    test.equal(mylexer.nextToken(), null);
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

exports['get less or equal sign'] = function (test) {
    var mylexer = lexer.createLexer('<=');
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, '<=');
    test.equal(token.type, TokenType.Sign);
    
    test.equal(mylexer.nextToken(), null);
}
