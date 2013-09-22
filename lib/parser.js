
var createLexer = require('./lexer').createLexer;
var TokenType = require('./lexer').TokenType;

function ConstantExpression(value) {
    this.compile = function () {
        return value;
    }
}

function InstanceVariableExpression(value) {
    this.compile = function () {
        return 'self.$' + value;
    }
}

function AssignExpression(name, expr) {
    this.compile = function () {
        return name + ' = ' + expr.compile() + ';';
    }
}

function LocalVariablesExpression(names) {
    this.compile = function () {
        var result = '';
        
        names.forEach(function (name) {
            if (result !== '')
                result += ' ';
            result += 'var ' + name + ';';
        });
        
        return result;
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
    
    this.parse = function(context) {
        var token = nextToken();
        
        if (token == null)
            return null;
            
        if (token.type === TokenType.Punctuation && token.value === '|')
            return parseLocalVariables(context);
            
        pushToken(token);
            
        var expr = parseTarget(context);
        
        if (token.type === TokenType.Name) {
            var token2 = nextToken();
            if (token2 && token2.value == ':=' && token2.type === TokenType.Sign && (expr instanceof ConstantExpression || expr instanceof LocalVariableExpression))
                return new AssignExpression(token.value, this.parse());
            else
                pushToken(token2);
        }

        return parseKeywordMessages(expr, context);
    }
    
    function parseLocalVariables(context)
    {
        var names = [];
        
        for (var token = nextToken(); token && token.type === TokenType.Name; token = nextToken()) {
            var name = token.value;
            context.defineLocalVariable(name);
            names.push(name);
        }
            
        return new LocalVariablesExpression(names);
    }
    
    function parseTarget(context) {
        var token = nextToken();
        
        if (token.type === TokenType.String)
            return new ConstantExpression("'" + token.value + "'");
            
        if (token.type === TokenType.Name && context && context.isInstanceVariable(token.value))
            return new InstanceVariableExpression(token.value);
            
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