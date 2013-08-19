
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
