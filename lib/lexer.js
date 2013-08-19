
var TokenType = { Name: 1 };

function Lexer(text) {
    var length = text ? text.length : 0;
    var position = 0;
    
    this.nextToken = function () {
        if (position < length) {
            position = length;
            return { value: text, type: TokenType.Name };
        }
            
        return null;
    };
}

function createLexer(text) {
    return new Lexer(text);
}

module.exports = {
    createLexer: createLexer,
    TokenType: TokenType
}