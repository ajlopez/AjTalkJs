
var TokenType = { Name: 1, Keyword: 2 };

function Lexer(text) {
    var length = text ? text.length : 0;
    var position = 0;
    
    this.nextToken = function () {
        while (position < length && isWhiteSpace(text[position]))
            position++;
            
        if (position >= length)
            return null;
            
        var result = text[position++];
        
        while (position < length && !isWhiteSpace(text[position]))
            result += text[position++];

        if (result[result.length - 1] == ':')
            return { value: result, type: TokenType.Keyword };
        
        return { value: result, type: TokenType.Name };
    };
}

function isWhiteSpace(ch) {
    return ch <= ' ';
}

function createLexer(text) {
    return new Lexer(text);
}

module.exports = {
    createLexer: createLexer,
    TokenType: TokenType
}