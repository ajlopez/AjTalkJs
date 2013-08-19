
var lexer = require('./lexer');

function Parser(text) {
    this.parse = function() {
        return {
            compile: function() { return text; }
        }
    }
}

function createParser(text) {
    return new Parser(text);
}

module.exports = {
    createParser: createParser
};