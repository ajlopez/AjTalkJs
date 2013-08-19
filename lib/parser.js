
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
        var expr = parseTarget();
                
        for (var token = lexer.nextToken(); token && token.type == TokenType.Name; token = lexer.nextToken())
            expr = new UnaryMessageExpression(expr, token.value);
            
        return expr;
    }
    
    function parseTarget() {
        var token = lexer.nextToken();
        
        if (token.type === TokenType.String)
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