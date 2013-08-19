
var createLexer = require('./lexer').createLexer;
var TokenType = require('./lexer').TokenType;

function ConstantExpression(value) {
    this.compile = function () {
        return value;
    }
}

function Parser(text) {
    var lexer = createLexer(text);
    
    this.parse = function() {
        var token = lexer.nextToken();
        
        if (token.type == TokenType.String)
            return new ConstantExpression("'" + token.value + "'");
            
        return new ConstantExpression(token.value);
    }
}

function createParser(text) {
    return new Parser(text);
}

module.exports = {
    createParser: createParser
};