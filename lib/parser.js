
var createLexer = require('./lexer').createLexer;
var TokenType = require('./lexer').TokenType;

function ConstantExpression(value) {
    this.compile = function () {
        return value;
    }
}

function UnaryMessageExpression(expr, selector) {
    this.compile = function () {
        return expr.compile() + "." + selector + "()";
    }
}

function Parser(text) {
    var lexer = createLexer(text);
    
    this.parse = function() {
        var token = lexer.nextToken();
        
        if (token.type === TokenType.String)
            return new ConstantExpression("'" + token.value + "'");
            
        var value = token.value;
        
        if (token.type === TokenType.Name) {
            token = lexer.nextToken();
            
            if (token && token.type === TokenType.Name)
                return new UnaryMessageExpression(new ConstantExpression(value), token.value);
        }
            
        return new ConstantExpression(value);
    }
}

function createParser(text) {
    return new Parser(text);
}

module.exports = {
    createParser: createParser
};