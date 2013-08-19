
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

function BinaryMessageExpression(left, selector, right) {
    this.compile = function () {
        return "(" + left.compile() + " " + selector + " " + right.compile() + ")";
    }
}

function Parser(text) {
    var lexer = createLexer(text);
    var tokens = [];
    
    this.parse = function() {
        var expr = parseTarget();

        return parseBinaryMessages(expr);
    }
    
    function parseTarget() {
        var token = nextToken();
        
        if (token.type === TokenType.String)
            return new ConstantExpression("'" + token.value + "'");
            
        return new ConstantExpression(token.value);
    }
    
    function parseBinaryMessages(expr) {
        expr = parseUnaryMessages(expr);
        
        for (var token = nextToken(); token && token.type == TokenType.Sign; token = nextToken()) {
            var target = parseTarget();
            var right = parseUnaryMessages(target);
            
            expr = new BinaryMessageExpression(expr, token.value, right);
        }
        
        pushToken(token);
        
        return expr;
    }
    
    function parseUnaryMessages(expr) {
        for (var token = nextToken(); token && token.type == TokenType.Name; token = nextToken())
            expr = new UnaryMessageExpression(expr, token.value);
            
        pushToken(token);
            
        return expr;
    }
    
    function nextToken() {
        if (tokens.length)
            return tokens.pop();
            
        return lexer.nextToken();
    }
    
    function pushToken(token) {
        if (token)
            tokens.push(token);
    }
}

function createParser(text) {
    return new Parser(text);
}

module.exports = {
    createParser: createParser
};