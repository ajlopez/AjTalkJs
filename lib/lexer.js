
var TokenType = { Name: 1 };

function Lexer(text) {
    this.nextToken = function () {
        return { value: text, type: TokenType.Name };
    };
}

function createLexer(text) {
    return new Lexer(text);
}

module.exports = {
    createLexer: createLexer,
    TokenType: TokenType
}