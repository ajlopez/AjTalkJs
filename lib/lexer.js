
var TokenType = { Name: 1, Integer: 2, Real: 3, Keyword: 4, Symbol: 5, Punctuation: 6 };

var punctuations = "(),.|!";

function Lexer(text) {
    var length = text ? text.length : 0;
    var position = 0;
    
    this.nextToken = function () {
        while (position < length && isWhiteSpace(text[position]))
            position++;
            
        if (position >= length)
            return null;
            
        var result = text[position++];
        
        if (isDigit(result))
            return nextInteger(result);
            
        if (isPunctuation(result))
            return { value: result, type: TokenType.Punctuation };
            
        if (result === '#')
            return nextSymbol();
        
        if (result === "'")
            return nextString();
        
        while (position < length && !isWhiteSpace(text[position])) {
            var ch = text[position++];
            result += ch;
            
            if (ch === ':')
                break;
        }

        if (result[result.length - 1] == ':')
            return { value: result, type: TokenType.Keyword };
        
        return { value: result, type: TokenType.Name };
    };
    
    function nextInteger(ch) {
        var result = ch;
        
        while (position < length && isDigit(text[position]))
            result += text[position++];
        
        return { value: result, type: TokenType.Integer };
    }
    
    function nextSymbol() {
        var result = '';
        
        while (position < length && !isWhiteSpace(text[position]))
            result += text[position++];
        
        return { value: result, type: TokenType.Symbol };
    }
    
    function nextString() {
        var result = '';
        
        while (position < length && text[position] !== "'")
            result += text[position++];

        if (position >= length)
            throw 'unclosed string';

        position++;
        
        return { value: result, type: TokenType.String };
    }
}

function isPunctuation(ch) {
    return punctuations.indexOf(ch) >= 0;
}

function isWhiteSpace(ch) {
    return ch <= ' ';
}

function isDigit(ch) {
    return ch >= '0' && ch <= '9';
}

function createLexer(text) {
    return new Lexer(text);
}

module.exports = {
    createLexer: createLexer,
    TokenType: TokenType
}

