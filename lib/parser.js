
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

function KeywordMessageExpression(target, selector, expressions) {
    this.compile = function () {
        var name = selector.replace(/:/g, '_');
        var result = target.compile() + "." + name + "(";
        
        for (var k = 0; k < expressions.length; k++) {
            if (k)
                result += ", ";
                
            result += expressions[k].compile();
        }
        
        result += ")";
        return result;
    }
}

function Parser(text) {
    var lexer = createLexer(text);
    var tokens = [];
    
    this.parse = function() {
        var expr = parseTarget();

        return parseKeywordMessages(expr);
    }
    
    function parseTarget() {
        var token = nextToken();
        
        if (token.type === TokenType.String)
            return new ConstantExpression("'" + token.value + "'");
            
        return new ConstantExpression(token.value);
    }
    
    function parseKeywordMessages(expr) {
        expr = parseBinaryMessages(expr);
        var selector = '';
        var exprs = [];
        
        for (var token = nextToken(); token && token.type == TokenType.Keyword; token = nextToken()) {
            selector += token.value;
            var target = parseTarget();
            exprs.push(parseBinaryMessages(target));
        }
        
        pushToken(token);
        
        if (exprs.length > 0)
            return new KeywordMessageExpression(expr, selector, exprs);
        
        return expr;
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