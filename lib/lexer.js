
var lexer = (function() {
    var TokenType = { Name: 1, Integer: 2, Real: 3, Keyword: 4, Symbol: 5, Punctuation: 6, Sign: 7, Character: 8, Parameter: 9 };

    var punctuations = "(),.|!";

    function Lexer(text) {
        var length = text ? text.length : 0;
        var position = 0;
        
        this.nextToken = function () {
            while (position < length) {
                var ch = text[position];
                
                if (isWhiteSpace(ch)) {
                    position++;
                    continue;
                }
                    
                if (ch === '"') {
                    position++;
                    skipComment();
                    continue;
                }
                
                break;
            }
                
            if (position >= length)
                return null;
                
            position++;
            
            if (isDigit(ch))
                return nextInteger(ch);
                
            if (isPunctuation(ch))
                return { value: ch, type: TokenType.Punctuation };
                
            if (ch === '#')
                return nextSymbol();
            
            if (ch === "'")
                return nextString();
                
            if (ch == "$")
                return nextCharacter();
                
            if (ch == ':' && isLetter(text[position]))
                return nextParameter();
            
            if (isLetter(ch))
                return nextName(ch);
            
            var result = ch;
            
            while (position < length && isSign(text[position]))
                result += text[position++];
                
            return { value: result, type: TokenType.Sign };
        };
        
        function skipComment() {
            while (position < length && text[position] !== '"')
                position++;
                
            position++;
        }
        
        function nextName(ch) {
            var result = ch;
            
            while (position < length && !isWhiteSpace(text[position])) {
                var ch2 = text[position++];
                result += ch2;
                
                if (ch2 === ':')
                    break;
            }

            if (result[result.length - 1] == ':')
                return { value: result, type: TokenType.Keyword };
            
            return { value: result, type: TokenType.Name };
        }
        
        function nextParameter() {
            var tokname = nextName('');
            
            return { value: tokname.value, type: TokenType.Parameter };
        }
            
        function nextCharacter()
        {
            var next = text[position++];
                
            return { value : next, type: TokenType.Character };
        }
        
        function nextInteger(ch) {
            var result = ch;
            
            while (position < length && isDigit(text[position]))
                result += text[position++];
                
            if (text[position] === '.') {
                position++;
                return nextReal(result + '.');
            }
                
            return { value: result, type: TokenType.Integer };
        }
        
        function nextReal(result) {
            while (position < length && isDigit(text[position]))
                result += text[position++];
                
            return { value: result, type: TokenType.Real };
        }
        
        function nextSymbol() {
            var result = '';
            
            while (position < length && !isWhiteSpace(text[position]) && !isPunctuation(text[position]))
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

    function isSign(ch) {
        return !isWhiteSpace(ch) && !isPunctuation(ch) && !isDigit(ch) && !isLetter(ch);
    }

    function isLetter(ch) {
        return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z');
    }

    function isDigit(ch) {
        return ch >= '0' && ch <= '9';
    }

    function createLexer(text) {
        return new Lexer(text);
    }

    return {
        createLexer: createLexer,
        TokenType: TokenType
    }
})();

if (typeof module !== 'undefined' && module && module.exports)
    module.exports = lexer;
