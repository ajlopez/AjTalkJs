
var lexer = require('../lib/lexer');

var TokenType = lexer.TokenType;

exports['get name'] = function (test) {
    var mylexer = lexer.createLexer("a");
    
    var token = mylexer.nextToken();
    
    test.ok(token);
    test.equal(token.value, "a");
    test.equal(token.type, TokenType.Name);
}