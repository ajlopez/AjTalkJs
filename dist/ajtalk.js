
var ajtalk = (function() {

var lexer = (function() {
    var TokenType = { Name: 1, String: 2, Integer: 3, Real: 4, Keyword: 5, Symbol: 6, Punctuation: 7, Sign: 8, Character: 9, Parameter: 10, Code: 11, Comment: 12 };

    var punctuations = "(),.|![]{};";

    function Lexer(text) {
        var length = text ? text.length : 0;
        var position = 0;
        var lasttoken = null;
        
        this.nextToken = function () {
            if (lasttoken != null) {
                var value = lasttoken;
                lasttoken = null;
                return value;
            }
            
            while (position < length) {
                var ch = text[position];
                
                if (isWhiteSpace(ch)) {
                    position++;
                    continue;
                }
                    
                if (ch === '"') {
                    position++;
                    var comment = nextComment();
                    
                    if (comment.value && comment.value.substring(0, 3) == 'js:')
                        return { value: comment.value.substring(3), type: TokenType.Code };
                        
                    continue;
                }
                
                break;
            }
                
            if (position >= length)
                return null;
                
            position++;
            
            if (isDigit(ch))
                return nextInteger(ch);
                
            if (ch === '-' && isDigit(text[position]))
                return nextInteger(ch);
                
            if (isPunctuation(ch))
                return { value: ch, type: TokenType.Punctuation };
                
            if (ch === '#' && text[position] === '(') {
                position++;
                return { value: '#(', type: TokenType.Punctuation };
            }
            
            if (ch === '#')
                return nextSymbol();
            
            if (ch === "'")
                return nextString();
                
            if (ch == "$")
                return nextCharacter();
                
            if (ch == "^")
                return { value: '^', type: TokenType.Sign };
                
            if (ch == ':' && isLetter(text[position]))
                return nextParameter();
            
            if (isLetter(ch))
                return nextName(ch);
            
            var result = ch;
            
            while (position < length && isSign(text[position]))
                result += text[position++];
                
            return { value: result, type: TokenType.Sign };
        };
        
        this.pushToken = function (token) {
            lasttoken = token;
        }
        
        function skipComment() {
            while (position < length && text[position] !== '"')
                position++;
                
            position++;
        }
        
        function nextName(ch) {
            var result = ch;
            
            while (position < length && !isWhiteSpace(text[position]) && !isPunctuation(text[position])) {
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
                
            if (text[position] === '.' && isDigit(text[position + 1])) {
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
            
            while (position < length) {
                if (text[position] === "'") {
                    if (text[position + 1] !== "'")
                        break;
                        
                    position++;
                }
                        
                result += text[position++];
            }

            if (text[position] != "'")
                throw 'unclosed string';
                
            position++;
            
            return { value: result, type: TokenType.String };
        }
        
        function nextComment() {
            var result = '';
            
            while (position < length) {
                if (text[position] === '"') {
                    if (text[position + 1] !== '"')
                        break;
                        
                    position++;
                }
                        
                result += text[position++];
            }

            if (text[position] != '"')
                throw 'unclosed comment';
                
            position++;
            
            return { value: result, type: TokenType.Comment };
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

if (typeof require != 'undefined')
	var lexer = require('./lexer');

var parser = (function () {
    var createLexer = lexer.createLexer;
    var TokenType = lexer.TokenType;

    function ConstantExpression(value) {
        this.compile = function () {
            return value;
        }
    }

    function VariableExpression(name) {
        this.compile = function () {
            return name;
        }
    }

    function GlobalVariableExpression(name) {
        this.compile = function () {
            if (name === 'Smalltalk')
                return name;
                
            return 'Smalltalk.' + name;
        }
    }

    function InstanceVariableExpression(name) {
        this.compile = function () {
            return 'self.$' + name;
        }
    }

    function AssignExpression(leftexpr, expr) {
        this.compile = function () {
            return leftexpr.compile() + ' = ' + expr.compile() + ';';
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
                if (token2 && token2.value == ':=' && token2.type === TokenType.Sign)
                    return new AssignExpression(expr, this.parse());
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
                
            if (token.type === TokenType.Name) {
                if (!context)
                    return new VariableExpression(token.value);
                    
                if (context.isInstanceVariable(token.value))
                    return new InstanceVariableExpression(token.value);
                    
                if (context.isLocalVariable(token.value) || context.isArgumentVariable(token.value))
                    return new VariableExpression(token.value);
                    
                return new GlobalVariableExpression(token.value);
            }
                
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

    return {
        createParser: createParser
    }
})();

if (typeof module !== 'undefined' && module && module.exports)
    module.exports = parser;
var chunkreader = (function() {	// Chunk Reader		function ChunkReader(text)	{		var position = 0;				this.nextChunk = function() {            if (text == null || text.length == 0)                return null;                            text = skipSpaces(text);            			if (text.length == 0)            {                text = null;				return '';            }							var result = '';                        if (text[0] == '!')            {                if (text.length > 1 && text[1] > ' ')                {                    result = '!';                    text = text.slice(1);                }                else                {                    text = text.slice(1);                    return '';                }            }            			var bangpos = text.indexOf("!");						if (bangpos < 0)			{				result += text;				text = null;				return result;			}						var bang2pos = text.indexOf("!!");						if (bang2pos == bangpos) {				while (bangpos >= 0 && bang2pos == bangpos)				{					result += text.slice(0, bangpos+1);					text = text.slice(bangpos + 2);					bangpos = text.indexOf("!");					bang2pos = text.indexOf("!!");				}								if (bangpos < 0)				{					result += text;					text = null;					return result;				}								result += text.slice(0, bangpos);				text = text.slice(bangpos + 1);								return result;			}						result += text.slice(0, bangpos);			text = text.slice(bangpos + 1);						return result;		}	}		ChunkReader.prototype.process = function(compiler) 	{		var chunk = this.nextChunk();		var ismethod = false;				while (chunk != null)		{			var isreader = false;						if (chunk != null && chunk.length > 0 && chunk[0] == '!')			{				chunk = chunk.slice(1);				isreader = true;			}						var result = compiler.compileBlock(chunk);			result = result.apply();						if (isreader)				result.scanFrom(this, true);							chunk = this.nextChunk();		}		}	    function skipSpaces(text)    {        var l = text.length;                for (var k=0; k < l; k++)            if (text[k] > ' ')                break;                        if (k==0)            return text;                    return text.slice(k);    }            function skipNewLines(text)    {        var l = text.length;                for (var k=0; k < l; k++)            if (text[k] >= ' ')                break;                        if (k==0)            return text;                    return text.slice(k);    }    return {        createReader: function (text) { return new ChunkReader(text); }     }})();if (typeof module !== 'undefined' && module && module.exports)    module.exports = chunkreader;
var superobj = { };

var topenv;

if (typeof window === 'undefined')
    topenv = global;
else
    topenv = window;

var machine = (function(Smalltalk) {
	var ByteCodes = {
		GetValue: 0,
		GetArgument: 1,
		GetLocal: 2,
		GetInstanceVariable: 3,
		GetGlobalVariable: 4,
		GetSelf: 5,
		GetNull: 6,
		GetBlock: 7,
        GetTrue: 8,
        GetFalse: 9,
		
		SetLocal: 10,
		SetInstanceVariable: 11,
		SetGlobalVariable: 12,
        GetYourself: 13,
        GetSuper: 14,
		
		Add: 20,
		Subtract: 21,
		Multiply: 22,
		Divide: 23,
		Concatenate: 24,
        Equal: 25,
        NotEqual: 26,
        StrictEqual: 27,
        Less: 28,
        Greater: 29,
        LessEqual: 30,
        GreaterEqual: 31,
		
		Primitive: 35,
		NativePrimitive: 36,
		LastTarget: 37,
		
		SendMessage: 40,
		NewList: 41,
		Return: 50,
        
        IfTrue: 51,
        IfFalse: 52,
        IfTrueIfFalse: 53,
        IfFalseIfTrue: 54,
		
		// Javascript bytecodes
		
		NativeAt: 100,
		NativeAtPut: 101,
		NativeApply: 102,
		NativeNew: 103,
        NativeDo: 104
	};
    	
	function Block(arity, nlocals, machine)
	{
        arity = arity || 0;
        nlocals = nlocals || 0;
        
		this.arity = arity;
		this.nlocals = nlocals;
        
		this.bytecodes = [];
		this.values = [];
        this.machine = machine;
	};
	
	Block.prototype.apply = function(self, args) 
	{
		return (new ExecutionBlock(this, self, args, null, this.machine)).execute();
	};
	
	Block.prototype.compileByteCode = function(bytecode, param)
	{
		this.bytecodes.push(bytecode);
		if (param != null)
			this.bytecodes.push(param);
	};
	
	Block.prototype.asFunction = function()
	{
		var block = this;
        var result;
        
        if (this.fn)
            result = function() { return block.fn.apply(this, arguments); }
        else
            result = function() { return block.apply(this, arguments); }
        
		result.method = this;
		return result;
	}
	
	Block.prototype.addValue = function(value)
	{
		var position = this.values.indexOf(value);
		
		if (position >= 0)
			return position;
			
		position = this.values.length;
		
		this.values.push(value);
		
		return position;
	}
	
	function ExecutionBlock(block, self, args, locals, machine)
	{
		this.block = block;
		this.self = self;
		this.args = args;
        this.machine = machine;
        
        if (locals)
            this.locals = locals;
        else
            this.locals = new Array(block.nlocals);
	}
	
	ExecutionBlock.prototype.executeWithParameters = function(parameters)
	{
		this.parameters = parameters;
		return this.execute();
	}
    
    ExecutionBlock.prototype.asFunction = function () {
        var exeblock = this;
        
        return function () {
            return exeblock.executeWithParameters(arguments);
        }
    }
    
    ExecutionBlock.prototype.value = function () {
        return this.execute();
    }
    
    ExecutionBlock.prototype.value_ = function (parameter) {
        return this.executeWithParameters([parameter]);
    }
	
	ExecutionBlock.prototype.execute = function()
	{
		var ip = 0;
		var bc = this.block.bytecodes;
		var l = bc.length;
		var stack = [];
		
		while (ip < l)
		{
			var bytecode = bc[ip++];
			
			switch(bytecode) {
				case ByteCodes.GetValue:
					var nv = bc[ip++];
					stack.push(this.block.values[nv]);
					break;
				case ByteCodes.GetBlock:
					var nv = bc[ip++];
					stack.push(new ExecutionBlock(this.block.values[nv], this.self, this.args, this.locals, this.machine));
					break;
				case ByteCodes.GetParameter:
					var np = bc[ip++];
					stack.push(this.parameters[np]);
					break;
				case ByteCodes.GetArgument:
					var na = bc[ip++];
					stack.push(this.args[na]);
					break;
				case ByteCodes.GetLocal:
					var nl = bc[ip++];
					stack.push(this.locals[nl]);
					break;
				case ByteCodes.SetLocal:
					var nl = bc[ip++];
                    this.locals[nl] = stack.pop();
					break;
				case ByteCodes.GetInstanceVariable:
					var niv = bc[ip++];
					stack.push(this.self[this.block.values[niv]]);
					break;
				case ByteCodes.GetGlobalVariable:
					nv = bc[ip++];
                    var varname = this.block.values[nv];
                    var result = this.machine.Smalltalk[varname];
                    
                    if (result === undefined)
                        result = topenv[varname];
                        
					stack.push(result);
					break;
				case ByteCodes.SetInstanceVariable:
					niv = bc[ip++];
					this.self[this.block.values[niv]] = stack.pop();
					break;
				case ByteCodes.SetGlobalVariable:
					nv = bc[ip++];
					this.machine.Smalltalk[this.block.values[nv]] = stack.pop();
					break;
				case ByteCodes.GetSelf:
					stack.push(this.self);
					break;
				case ByteCodes.GetSuper:
					stack.push(superobj);
					break;
				case ByteCodes.GetNull:
					stack.push(null);
					break;
				case ByteCodes.GetTrue:
					stack.push(true);
					break;
				case ByteCodes.GetFalse:
					stack.push(false);
					break;
				case ByteCodes.LastTarget:
					stack.push(target);
					break;
				case ByteCodes.GetYourself:
					break;
				case ByteCodes.Concatenate:
					var op2 = stack.pop();
					var target = stack.pop();
					stack.push(target.toString() + op2.toString());
					break;
				case ByteCodes.Equal:
					var op2 = stack.pop();
					var target = stack.pop();
					stack.push(target == op2);
					break;
				case ByteCodes.StrictEqual:
					var op2 = stack.pop();
					var target = stack.pop();
					stack.push(target === op2);
					break;
				case ByteCodes.NotEqual:
					var op2 = stack.pop();
					var target = stack.pop();
					stack.push(target != op2);
					break;
				case ByteCodes.Less:
					var op2 = stack.pop();
					var target = stack.pop();
					stack.push(target < op2);
					break;
				case ByteCodes.Greater:
					var op2 = stack.pop();
					var target = stack.pop();
					stack.push(target > op2);
					break;
				case ByteCodes.LessEqual:
					var op2 = stack.pop();
					var target = stack.pop();
					stack.push(target <= op2);
					break;
				case ByteCodes.GreaterEqual:
					var op2 = stack.pop();
					var target = stack.pop();
					stack.push(target >= op2);
					break;
				case ByteCodes.IfTrue:
					var blk = stack.pop();
					var target = stack.pop();
                    
                    if (target)
                        stack.push(blk.execute());
                    else
                        stack.push(null);
                        
					break;
				case ByteCodes.IfTrueIfFalse:
					var blk2 = stack.pop();
					var blk1 = stack.pop();
					var target = stack.pop();
                    
                    if (target)
                        stack.push(blk1.execute());
                    else
                        stack.push(blk2.execute());
                        
					break;
				case ByteCodes.IfFalse:
					var blk = stack.pop();
					var target = stack.pop();
                    
                    if (!target)
                        stack.push(blk.execute());
                    else
                        stack.push(null);
                        
					break;
				case ByteCodes.IfFalseIfTrue:
					var blk2 = stack.pop();
					var blk1 = stack.pop();
					var target = stack.pop();
                    
                    if (!target)
                        stack.push(blk1.execute());
                    else
                        stack.push(blk2.execute());
                        
					break;
				case ByteCodes.Add:
					var op2 = stack.pop();
					var target = stack.pop();
					stack.push(target + op2);
					break;
				case ByteCodes.Subtract:
					var op2 = stack.pop();
					var target = stack.pop();
					stack.push(target - op2);
					break;
				case ByteCodes.Multiply:
					var op2 = stack.pop();
					var target = stack.pop();
					stack.push(target * op2);
					break;
				case ByteCodes.Divide:
					var op2 = stack.pop();
					var target = stack.pop();
					stack.push(target / op2);
					break;
				case ByteCodes.Return:
					return stack.pop();
					break;
				case ByteCodes.Primitive:
					var nprim = bc[ip++];
					return Primitives[nprim](this);
					break;
				case ByteCodes.NativePrimitive:
					var nv = bc[ip++];
					var natprim = this.block.values[nv];
					stack.push(eval(natprim));
					break;
                case ByteCodes.NativeDo:
                    var blk = stack.pop();
                    var target = stack.pop();
                    
                    for (var n in target) {
                        var item = target[n];
                        blk.executeWithParameters([item]);
                    }
                    
                    break;
				case ByteCodes.SendMessage:
					var arity = bc[ip++];
					var selector = stack.pop();
					var args = [];
					
					for (var k = 0; k < arity; k++)
						args.unshift(stack.pop());
						
					var target = stack.pop();

					if (!target)
						target = this.machine.Smalltalk.Nil;

                    var fn;

                    if (target === superobj) {
                        fn = this.block.class.super.func.prototype[selector];
                        target = this.self;
                    }
                    else {
                        fn = target[selector];
                        
                        if (!fn && !target.klass) {
                            var p = selector.indexOf("_");
                            
                            if (p > 0)
                                fn = target[selector.substring(0, p)];
                            else
                                fn = target[selector];
                        }
                    }
                    
                    if (!fn)
                        if (target.doesNotUnderstand_)
                            stack.push(target.doesNotUnderstand_({ selector: selector, arguments: args }));
                        else if (selector == 'perform_with_' || selector == 'perform_with_delayed_') {
                            fn = target[args[0]];
                            if (!fn)
                                throw "unknown selector '" + args[0] + "'";
                                
                            if (args[2]) {
                                stack.push(null);
                                setTimeout(function () { fn.apply(target, args[1]); });
                            }
                            else
                                stack.push(fn.apply(target, args[1]));
                        }
                        else
                            throw "unknown selector '" + selector + "'";
                    else if (fn.apply)
                        stack.push(fn.apply(target, args));
                    else
                        stack.push(fn);
					
					break;
				case ByteCodes.NativeNew:
					var args = stack.pop();
					var target = stack.pop();
                    
                    var newobj = Object.create(target.prototype);
                    
                    if (target == Error) {
                        target.prototype.constructor.apply(newobj, args);
                        stack.push(newobj);
                    }
                    else {
                        var result = target.prototype.constructor.apply(newobj, args);
                        stack.push(result);
                    }
					
					break;
				case ByteCodes.NativeApply:
					var args = stack.pop();
					var selector = stack.pop();						
					var target = stack.pop();
                    
                    var fn = target[selector];
                    
                    if (!fn)
                        throw "unknown native selector '" + selector + "'";
                    
					stack.push(fn.apply(target, args));
					
					break;
				case ByteCodes.NativeAt:
					var selector = stack.pop();
					var target = stack.pop();
					
					stack.push(target[selector]);
					
					break;
				case ByteCodes.NativeAtPut:
					var value = stack.pop();
					var selector = stack.pop();
					var target = stack.pop();
					
					target[selector] = value;
					
					stack.push(value);
					
					break;
				case ByteCodes.NewList:
					var nitems = bc[ip++];
					var result = [];
					
					for (var k = 0; k < nitems; k++)
						result.unshift(stack.pop());
					
					stack.push(result);
					
					break;
				default:
					throw "Invalid ByteCode " + bytecode
					breakl
			};
		}
		
        // TODO review stack.length > 0
		if (stack.length > 0)
			return stack.pop();
        else
            return this.self;
	}
    
    function Machine(Smalltalk) {
        this.Smalltalk = Smalltalk;
        this.createBlock = function (arity, nlocals) {
            return new Block(arity, nlocals, this);
        }
    }
    
    return {
        ByteCodes: ByteCodes,
        createMachine: function (Smalltalk) { return new Machine(Smalltalk); }
    }
})();

if (typeof module !== 'undefined' && module && module.exports)
    module.exports = machine;
if (typeof require != 'undefined') {
    var chunkreader = require('./chunkreader');
    var machine = require('./machine');
    var lexer = require('./lexer');
}

var ByteCodes = machine.ByteCodes;
var TokenType = lexer.TokenType;

var bccompiler = (function() {

    function isBinaryOperator(token) {
        return (token.type == TokenType.Sign && token.value[0] != ':') || (token.type == TokenType.Punctuation && (token.value==',' || token.value=='|'));    
    }

    function isNumber(token) {
        return token.type === TokenType.Integer || token.type === TokenType.Real;
    }
       
	function Compiler(machine)
	{
        this.machine = machine;
	}
	
	Compiler.prototype.compileBlock = function(text) 
	{
		var block = this.machine.createBlock(0, 0);
		var mylexer = lexer.createLexer(text);
		
		if (this.log)
			mylexer.log = true;
		
		this.compileExpressions(block, mylexer);
		
		return block;
	};
	
	Compiler.prototype.compileMethod = function(text, klass)
	{
		var mylexer = lexer.createLexer(text);
		var token = mylexer.nextToken();
        
		if (token == null)
			return null;
            
		mylexer.pushToken(token);
		var signature = this.compileMethodSignature(mylexer);		
		
		var method = this.machine.createBlock(signature.argnames.length, 0);
		method.klass = klass;
		method.argnames = signature.argnames;
		method.localnames = signature.localnames;
		method.code = text;		
		method.name = signature.name;
		
		this.compileExpressions(method, mylexer);
		
		return method;
	};
	
	Compiler.prototype.compileMethodSignature = function(lexer)
	{
		var signature = {
			name: '',
			argnames: [],
			localnames: []
		}
		
		var token = lexer.nextToken();
		
		if (token == null)
			throw "Method code expected";
			
		if (token.type == TokenType.Name)
		{
			signature.name = token.value;
			compileLocalNames(lexer, signature);
			return signature;
		}
				
		// TODO isBinaryOperator?
		if (token.type === TokenType.Sign || (token.type === TokenType.Punctuation && token.value === '|'))
		{
			signature.name = token.value;
			token = lexer.nextToken();
			if (token == null || token.type !== TokenType.Name)
				throw "Argument name expected";
			signature.argnames.push(token.value);
			compileLocalNames(lexer, signature);
			return signature;
		}
		
		while (token != null && token.type == TokenType.Keyword)
		{
			var keyword = token.value;
			signature.name += keyword;
			token = lexer.nextToken();
			if (token == null || token.type !== TokenType.Name)
				throw "Argument name expected";
			signature.argnames.push(token.value);
			token = lexer.nextToken();
		}
		
		if (token != null)
			lexer.pushToken(token);
		
		compileLocalNames(lexer, signature);

		return signature;
	}
	
	function compileLocalNames(lexer, signature)
	{
		var token = lexer.nextToken();
		
		if (token == null)
			return;
			
		if (token.type !== TokenType.Punctuation || token.value != '|')
		{
			lexer.pushToken(token);
			return;
		}
		
		token = lexer.nextToken();
		
		while (token != null && token.type == TokenType.Name)
		{
			signature.localnames.push(token.value);
			token = lexer.nextToken();
		}
		
		if (token.type !== TokenType.Punctuation || token.value != '|')
			throw "Expected '|'";
	}

	Compiler.prototype.compileExpressions = function(block, lexer, islist, isblock)
	{
		var oldtarget = this.target;
		this.target = null;
		var result = this.compileExpressionsInternal(block, lexer, islist, isblock);
		this.target = oldtarget;
		return result;
	}
	
    // TODO review arguments, too much?
	Compiler.prototype.compileExpressionsInternal = function(block, lexer, islist, isblock)
	{
		this.target = null;
		var token = lexer.nextToken();
		
		var nexprs = 0;
        
        while (isblock && token != null && token.type === TokenType.Parameter)
        {
            if (block.parameternames == null)
                block.parameternames = [];
            
            block.parameternames.push(token.value);
            token = lexer.nextToken();
        }
        
        if (isblock && block.parameternames != null && block.parameternames.length > 0)
            if (token.type !== TokenType.Punctuation || token.value != '|')
                throw "Expected '|'";
            else
                token = lexer.nextToken();
				
		if (isblock && token != null && token.type === TokenType.Punctuation && token.value == '|')
		{
			if (!block.localnames)
				block.localnames = [];
				
			token = lexer.nextToken();
			
			while (token != null && token.type === TokenType.Name)
			{
				block.localnames.push(token.value);
				token = lexer.nextToken();
			}
			
			if (token == null || token.type !== TokenType.Punctuation || token.value !== '|')
				throw "Expected '|'";
				
			token = lexer.nextToken();
		}
		
		while (token != null) 
		{
            if (token.type == TokenType.Code && !isblock) {
                var args = [];
                
                if (block.argnames)
                    args = block.argnames.slice();
                    
                args.push(token.value);
                
                block.fn = Function.constructor.apply(Function, args);
                return nexprs;
            }
            
			if (token.type == TokenType.Punctuation && token.value == '.')
			{
				token = lexer.nextToken();
				continue;
			}
			
			if (islist && token.type === TokenType.Punctuation && token.value == '}')
				return nexprs;
				
			if (isblock && token.type === TokenType.Punctuation && token.value == ']')
			{
				return nexprs;
			}
				
			if (token.type === TokenType.Punctuation && token.value == ';')
			{
				this.target = true;
				token = lexer.nextToken();
				continue;
			}
			
			lexer.pushToken(token);
			this.compileExpression(block, lexer);
			nexprs++;
			token = lexer.nextToken();
		}
	}
	
	Compiler.prototype.compileExpression = function(block, lexer)
	{
		return this.compileKeywordExpression(block, lexer);
	}
	
	Compiler.prototype.compileKeywordExpression = function(block, lexer)
	{
		this.compileBinaryExpression(block, lexer);
		
		var token = lexer.nextToken();
		
		if (token == null || token.type !== TokenType.Keyword) 
		{
			if (token != null)
				lexer.pushToken(token);
				
			return;
		}
		
		var selector = '';
		var arity = 0;
		
		while (token != null && token.type === TokenType.Keyword)
		{
			selector += token.value;
			arity++;
			this.compileBinaryExpression(block, lexer);
			token = lexer.nextToken();			
		}
		
		if (token != null)
			lexer.pushToken(token);
			
		switch (selector)
		{
			case 'ndo:':
				block.compileByteCode(ByteCodes.NativeDo);
				return;
			case 'nat:':
				block.compileByteCode(ByteCodes.NativeAt);
				return;
			case 'nat:put:':
				block.compileByteCode(ByteCodes.NativeAtPut);
				return;
			case 'napply:':
				block.compileByteCode(ByteCodes.GetNull);
				block.compileByteCode(ByteCodes.NativeApply);
				return;
			case 'napply:with:':
				block.compileByteCode(ByteCodes.NativeApply);
				return;
			case 'nnew:':
				block.compileByteCode(ByteCodes.NativeNew);
				return;
			case 'ifTrue:':
				block.compileByteCode(ByteCodes.IfTrue);
				return;
			case 'ifTrue:ifFalse:':
				block.compileByteCode(ByteCodes.IfTrueIfFalse);
				return;
			case 'ifFalse:':
				block.compileByteCode(ByteCodes.IfFalse);
				return;
			case 'ifFalse:ifTrue:':
				block.compileByteCode(ByteCodes.IfFalseIfTrue);
				return;
		}

        var mthselector = selector.replace(/:/g,'_');
		var position = block.addValue(mthselector);
		block.compileByteCode(ByteCodes.GetValue, position);
		block.compileByteCode(ByteCodes.SendMessage, arity);
	}
	
	Compiler.prototype.compileBinaryExpression = function(block, lexer)
	{
		this.compileUnaryExpression(block, lexer);
		
		var token = lexer.nextToken();
		
		while (token != null && (isBinaryOperator(token) || (isNumber(token) && token.value < 0)))
		{			
			if (isBinaryOperator(token))
				this.compileUnaryExpression(block, lexer);

			switch (token.value) 
			{
				case '+':
					block.compileByteCode(ByteCodes.Add);
					break;
				case '-':
					block.compileByteCode(ByteCodes.Subtract);
					break;
				case '*':
					block.compileByteCode(ByteCodes.Multiply);
					break;
				case '/':
					block.compileByteCode(ByteCodes.Divide);
					break;
				case ',':
					block.compileByteCode(ByteCodes.Concatenate);
					break;
				case '=':
					block.compileByteCode(ByteCodes.Equal);
					break;
				case '==':
					block.compileByteCode(ByteCodes.StrictEqual);
					break;
				case '~=':
					block.compileByteCode(ByteCodes.NotEqual);
					break;
				case '<':
					block.compileByteCode(ByteCodes.Less);
					break;
				case '>':
					block.compileByteCode(ByteCodes.Greater);
					break;
				case '<=':
					block.compileByteCode(ByteCodes.LessEqual);
					break;
				case '>=':
					block.compileByteCode(ByteCodes.GreaterEqual);
					break;
				default:
					if (isNumber(token))
					{
						position = block.addValue(-token.value);
						block.compileByteCode(ByteCodes.GetValue, position);
						block.compileByteCode(ByteCodes.Subtract);
					}
					else {
						var position = block.addValue(token.value);
						block.compileByteCode(ByteCodes.GetValue, position);
						block.compileByteCode(ByteCodes.SendMessage, 1);
					}
					break;
			};
			
			token = lexer.nextToken();
		}
		
		if (token != null)
			lexer.pushToken(token);
	}
	
	Compiler.prototype.compileUnaryExpression = function(block, lexer)
	{
		this.compileTerm(block, lexer);
		
		var token = lexer.nextToken();
		
		while (token != null && token.type === TokenType.Name)
		{
			switch (token.value)
			{
				case 'yourself':
					block.compileByteCode(ByteCodes.GetYourself);
					break;
				case 'nnew':
					block.compileByteCode(ByteCodes.GetNull);
					block.compileByteCode(ByteCodes.NativeNew);
					break;
				default:
					var position = block.addValue(token.value);
					block.compileByteCode(ByteCodes.GetValue, position);
					block.compileByteCode(ByteCodes.SendMessage, 0);
					break;
			}
			
			token = lexer.nextToken();
		}
		
		if (token != null)
			lexer.pushToken(token);
	}
	
	Compiler.prototype.compileTerm = function(block, lexer)
	{
		if (this.target)
		{
			this.target = null;
			block.compileByteCode(ByteCodes.LastTarget);
			return;
		}
			
		var token = lexer.nextToken();
				
		if (token == null)
			return;
			
		if (token.type === TokenType.Name) 
		{
            if (token.value == "self")
            {
                block.compileByteCode(ByteCodes.GetSelf);
                return;
            }
            
            if (token.value == "super")
            {
                block.compileByteCode(ByteCodes.GetSuper);
                return;
            }
            
            if (token.value == "nil")
            {
                block.compileByteCode(ByteCodes.GetNull);
                return;
            }
            
            if (token.value == "false")
            {
                block.compileByteCode(ByteCodes.GetFalse);
                return;
            }
            
            if (token.value == "true")
            {
                block.compileByteCode(ByteCodes.GetTrue);
                return;
            }
            
			var name = token.value;
			var position = 0;
			var islocal = false;
			var isargument = false;
			var isvariable = false;
            var isparameter = false;
            var varname = null;
			
			if (block.parameternames != null)
			{
				var index = block.parameternames.indexOf(name);
				
				if (index >= 0)
				{
					position = index;
					isparameter = true;
				}
			}
			
			if (!isparameter && block.argnames != null)
			{
				var index = block.argnames.indexOf(name);
				
				if (index >= 0)
				{
					position = index;
					isargument = true;
				}
			}
			
			if (!isparameter && !isargument && block.localnames != null)
			{
				index = block.localnames.indexOf(name);
				
				if (index >= 0) 
				{
					position = index;
					islocal = true;
				}
			}
			
			if (!isparameter && !isargument && !islocal && block.klass != null)
			{
                if (block.klass.instvarnames)
                    index = block.klass.instvarnames.indexOf(name);
                else
                    index = -1;
				
				if (index >= 0)
				{
                    varname = '$' + name;                    
					position = block.addValue(varname);
					isvariable = true;
				}
			}

			if (!isparameter && !isargument && !islocal && !isvariable)
				position = block.addValue(name);
			
			token = lexer.nextToken();
			
			if (isargument || token == null || token.type !== TokenType.Sign || token.value != ':=')
			{
				lexer.pushToken(token);

				if (isparameter)
					block.compileByteCode(ByteCodes.GetParameter, position);
				else if (isargument)
					block.compileByteCode(ByteCodes.GetArgument, position);
				else if (islocal)
					block.compileByteCode(ByteCodes.GetLocal, position);
				else if (isvariable)
					block.compileByteCode(ByteCodes.GetInstanceVariable, position);
				else
					block.compileByteCode(ByteCodes.GetGlobalVariable, position);
					
				return;
			}
			
			this.compileExpression(block, lexer);

			if (islocal)
				block.compileByteCode(ByteCodes.SetLocal, position);
			else if (isvariable)
				block.compileByteCode(ByteCodes.SetInstanceVariable, position);
			else
				block.compileByteCode(ByteCodes.SetGlobalVariable, position);
			
			return;
		}
		
		if (token.type === TokenType.Punctuation && token.value == '(')
		{
			this.compileExpression(block, lexer);
			token = lexer.nextToken();
            if (token == null)
				throw "Expected ')' instead of end of input";
			if (token.type !== TokenType.Punctuation || token.value !== ')')
				throw "Expected ')' instead of '" + token.value + "'";
			return;
		}
		
		if (token.type == TokenType.Punctuation && token.value == '{')
		{
			var nexprs = this.compileExpressions(block, lexer, true);
			block.compileByteCode(ByteCodes.NewList, nexprs);
			return;
		}

        // TODO review access of instance variables
        // TODO review arity, nlocals use
		if (token.type === TokenType.Punctuation && token.value == '[')
		{
            var blk = this.machine.createBlock(0, 0);
            blk.argnames = block.argnames;
            if (block.localnames)
                blk.localnames = block.localnames.slice(0);
            else
                blk.localnames = [];
                
            this.compileExpressions(blk, lexer, false, true);
            position = block.addValue(blk);
			block.compileByteCode(ByteCodes.GetBlock, position);
			return;
		}
		
		// Constant Array
		if (token.type === TokenType.Punctuation && token.value == '#(')
		{
			this.compileConstantArray(block, lexer);
			return;
		}
        
		// TODO review symbol treatment
		if (token.type === TokenType.String || isNumber(token) || token.type === TokenType.Symbol || token.type === TokenType.Character)
		{
            var value = token.value;
            
            if (token.type === TokenType.Integer)
                value = parseInt(value);
            else if (token.type === TokenType.Real)
                value = parseFloat(value);
                
			var position = block.addValue(value);
			block.compileByteCode(ByteCodes.GetValue, position);
			return;
		}
		
		if (token.type === TokenType.Sign && token.value == '^')
		{
			this.compileExpression(block, lexer);
			block.compileByteCode(ByteCodes.Return);
			return;
		}
		
		if (token.type == TokenType.Sign && token.value == '<')
		{
			token = lexer.nextToken();
			
			if (token.type !== TokenType.Keyword || token.value != 'primitive:')
				throw "Invalid term " + token.value;
				
			token = lexer.nextToken();
			
			if (token.type === TokenType.String)
			{
				block.compileByteCode(ByteCodes.NativePrimitive, block.addValue(token.value));

				token = lexer.nextToken();
				
				if (token.type === TokenType.Keyword && token.value == 'module:')
				{
					token = lexer.nextToken();
					if (token == null || token.type !== TokenType.String)
						throw "Invalid primitive";
					token = lexer.nextToken();
				}
				
				if (token.type !== TokenType.Sign || token.value != '>')
					throw "Invalid primitive " + token.value;
					
				return;
			}
			
			if (token.type != TokenType.Integer)
				throw "Invalid term " + token.value;
				
			block.compileByteCode(ByteCodes.Primitive, parseInt(token.value));
			return;
		}

		throw "Invalid term " + token.value;
	}
	
	Compiler.prototype.compileConstantArray = function(block, lexer)
	{
		var nelements = 0;
		
		for (var token = lexer.nextToken(); token != null && (token.type !== TokenType.Punctuation || token.value !=')'); token = lexer.nextToken())
		{
			if (token.type === TokenType.Punctuation && token.value == '(')
				this.compileConstantArray(block,lexer);
			else {
				var position = block.addValue(token.value);
				block.compileByteCode(ByteCodes.GetValue, position);				
			}
			
			nelements++;
		}
		
		block.compileByteCode(ByteCodes.NewList, nelements);
	}
	
	Compiler.prototype.compileName = function(lexer)
	{
		var token = lexer.nextToken();
		if (token == null || token.type !== TokenType.Name)
			throw "Name expected";
		return token.value;
	}
    
    return {
        createCompiler: function (machine) { return new Compiler(machine); }
    };
})();

if (typeof module !== 'undefined' && module && module.exports)
    module.exports = bccompiler;

if (typeof require != 'undefined') {
    var bccompiler = require('./bccompiler');
    var chunkreader = require('./chunkreader');
    var fs = require('fs');
    var path = require('path');
    var machine = require('./machine');
    var mod = require('module');
}
	
var hasjquery = (typeof jQuery != 'undefined');

var ajtalk = (function() {

	// define new prototype functionss
	
	function extend(klass, name, func)
	{
		if (hasjquery)
		{
			Object.defineProperty(klass.prototype, name, func);
		}
		else
		{
			klass.prototype[name] = func;
		}
	}

    // Object new methods
/*	
	extend(Object, "sendMessage", function(selector, args)
    {
        return this[selector].apply(this, args);
    });
    
	extend(Object, "nat_", function(name)
    {
        return this[name];
    });
    
	extend(Object, "nat_put_", function(name, value)
    {
        this[name] = value;
        return value;
    });
    
	extend(Object, "napply_", function(name)
    {
        return this[name].apply(this);
    });
    
	extend(Object, "napply_with_", function(name, args)
    {
        return this[name].apply(this, args);
    });
    
	extend(Object, "ifNil_", function(block)
	{
	});
*/    
    // Number new methods

    Number.prototype.max_ = function(x) {
        if (this >= x)
            return this;
        return x;
    }

    Number.prototype.min_ = function(x) {
        if (this <= x)
            return this;
        return x;
    }

    Number.prototype['*'] = function(x) {
        return this * x;
    }
       
    Number.prototype['+'] = function(x) {
        return this + x;
    }
    
    Number.prototype['@'] = function(x) {
        var point = Smalltalk.Point.basicNew();
        point.setX_setY_(this, x);
        return point;
    }

	Number.prototype['do_'] = function(exeblock) {
		if (exeblock.executeWithParameters)
		{
			for (var k=1; k<=this; k++)
				exeblock.executeWithParameters([k]);
		}
		else
		{
			for (var k=1; k<=this; k++)
				exeblock.apply(exeblock, [k]);
		}
	}
	
    Number.prototype.asFloat = function() {
        return parseFloat(this);
    }
    
    Number.prototype.isInteger = function() {
        return this % 1 == 0;
    }

    Number.prototype.degreesToRadians = function() {
        return this * 2 * Math.PI / 360;
    }

    Number.prototype.cos = function() {
        return Math.cos(this);
    }

    Number.prototype.sin = function() {
        return Math.sin(this);
    }

    Number.prototype.rounded = function() {
        return Math.round(this);
    }

    // Function new methods
    
    Function.prototype.nnew = function()
    {
        return new this;
    }
    
    Function.prototype.nnew_ = function(args)
    {
        return this.prototype.constructor.apply(this, args);
    }
    
    Function.prototype.value = function () {
        return this();
    }
    
    Function.prototype.value_ = function (arg) {
        return this(arg);
    }
    
    Function.prototype.value_with_ = function (arg1, arg2) {
        return this(arg1, arg2);
    }
	
    // Smalltalk variable

	var Smalltalk = {};
    
    var mach = machine.createMachine(Smalltalk);
    
    Smalltalk.Machine = mach;
    
    if (typeof window !== 'undefined')
        Smalltalk.Global = window;
    else
        Smalltalk.Global = global;
        
    Smalltalk.NativeObject = Object;
    Object.new = function () { return { } };
    /*
    Object.prototype.toArray = function () {
        var result = [];
        
        for (n in this)
            result.push(this[n]);
            
        return result;
    };
    */
    
    Smalltalk.NativeArray = Array;
    Array.new = function () { return [ ] };
    Array.new_ = function (size) { return Array(size); };
    
    Smalltalk.Smalltalk = Smalltalk;
    
    Smalltalk.Error = {
        signal_: function (message) {
            throw new Error(message);
        }
    }
    
    function strequire(name) {
        var modfolders = mod._nodeModulePaths(process.cwd());
        
        for (var k = 0; k < modfolders.length; k++) {
            filename = path.join(modfolders[k], 'ajtalkjs-' + name, 'Init.st');

            if (!fs.existsSync(filename)) {
                filename = path.join(modfolders[k], name);
                if (fs.existsSync(filename))
                    return require(filename);
                    
                continue;
            }

            return load(filename);
        }
        
        return require(name);
    };
    
    Smalltalk.require_ = strequire;
        
    function createMetaclass(name, supermetaklass, clsvarnames)
    {
        var protometaklass = new Function();
        
        if (supermetaklass)
        {
			// Chain metaclass prototypes
            protometaklass.prototype.__proto__ = supermetaklass.proto;
        }
        else
        {
			// First metaclass methods
        }
        
        var metaklass = new protometaklass;
		
		// Function with prototype of this metaklass instances
        metaklass.func = new Function();
        metaklass.proto = protometaklass.prototype;
        metaklass.$name = name;
        metaklass.super = supermetaklass;
        metaklass.instvarnames = clsvarnames;
        
        metaklass.func.prototype.klass = metaklass;
		
		if (supermetaklass) 
		{
			// Chaining instances prototypes
            metaklass.func.prototype.__proto__ = supermetaklass.func.prototype;
		}
        else 
		{   
			// First instance methods
		}
        
        Smalltalk[name] = metaklass;
        
        return metaklass;
    }

    function createClass(name, superklass, instvarnames, clsvarnames)
    {
		var protometaklass = createMetaclass(name + ' class', superklass ? superklass.klass : null, clsvarnames);
        var protoklass = protometaklass.func;
        
        if (superklass)
        {
			// Chain class prototypes
            protoklass.prototype.__proto__ = superklass.proto;
        }
        else
        {
			// TODO Tricky fist metaclass inherits first class (it should be Object)
			protometaklass.proto.__proto__ = protoklass.prototype;
			
			// First class methods
            protoklass.prototype.basicNew = function()
            {
                var obj = new this.func;
                obj.klass = this;
                return obj;
            }
            
            protoklass.prototype.new = function()
            {
				return this.new();
            }
            
            protoklass.prototype.defineSubclass = function(name, instvarnames, clsvarnames)
            {
                return createClass(name, this, instvarnames, clsvarnames);
            }
            
            protoklass.prototype.defineMethod = function(name, method)
            {
                var mthname = name.replace(/:/g, '_');

                if (typeof method == "function")
                    this.func.prototype[mthname] = method;
                else {
                    method.class = this;
                    this.func.prototype[mthname] = method.asFunction();
                }
            }
            
            protoklass.prototype.defineClassMethod = function(name, method)
            {
                var mthname = name.replace(/:/g, '_');
                if (typeof method == "function")
                    this.proto[mthname] = method;
                else
                    this.proto[mthname] = method.asFunction();
            }

            protoklass.prototype.copyClassMethod = function(fromname, toname)
            {
                var frommthname = fromname.replace(/:/g, '_');
                var tomthname = toname.replace(/:/g, '_');
                this.proto[tomthname] = this.proto[frommthname];
            }
        }
        
        var klass = new protoklass;
		
		// Function with prototype of this klass instances
        klass.func = new Function();
        klass.proto = protoklass.prototype;
        klass.$name = name;
        klass.super = superklass;
        klass.instvarnames = instvarnames;
        klass.clsvarnames = clsvarnames;
		klass.klass = protometaklass;
        
        klass.func.prototype.klass = klass;
		
		if (superklass) 
		{
			// Chaining instances prototypes
            klass.func.prototype.__proto__ = superklass.func.prototype;
		}
        else 
		{   
			// TODO Tricky first class inherits Object
			klass.proto.__proto__ = klass.func.prototype;
			
			// First instance methods
            klass.func.prototype.sendMessage = function(selector, args)
            {
                return this[selector].apply(this, args);
            }
		}
        
        Smalltalk[name] = klass;
        
        return klass;
    }
    
    createClass('ProtoObject');
	createClass('Object', Smalltalk.ProtoObject);
	createClass('Array', Smalltalk.Object);
	createClass('Nil', Smalltalk.Object);
	
	Smalltalk.ProtoObject.defineMethod('class', function()
		{
			return this.klass;
		});

	// TODO process argument
	Smalltalk.ProtoObject.defineClassMethod('superclass:', function(superclass)
		{
		});

	Smalltalk.ProtoObject.defineClassMethod('compileMethod:', function(text)
		{
			var compiler = bccompiler.createCompiler(mach);
			var method = compiler.compileMethod(text, this);
			this.defineMethod(method.name, method);
			return method;
		});
	
	Smalltalk.ProtoObject.defineClassMethod('name', function()
		{
			return this.$name;
		});
	
	Smalltalk.ProtoObject.defineClassMethod('compileClassMethod:', function(text)
		{
			var compiler = bccompiler.createCompiler(mach);
			var method = compiler.compileMethod(text, this);
			this.defineClassMethod(method.name, method);
			return method;
		});
		
	Smalltalk.ProtoObject.defineClassMethod('subclass:instanceVariableNames:classVariableNames:',
		function (name, instvarnames, clsvarnames)
		{
			// TODO Quick hack to suppor .st load
			if (name == 'ProtoObject' || name == 'Object')
				return;

			createClass(name, this, toNames(instvarnames), toNames(clsvarnames));
		});

    // TODO process extra arguments
    Smalltalk.ProtoObject.copyClassMethod('subclass:instanceVariableNames:classVariableNames:','subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:');
    Smalltalk.ProtoObject.copyClassMethod('subclass:instanceVariableNames:classVariableNames:','weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:');
	// TODO implement variable subclass
    Smalltalk.ProtoObject.copyClassMethod('subclass:instanceVariableNames:classVariableNames:','variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:');
		        
    // TODO process argument
	Smalltalk.ProtoObject.defineClassMethod('commentStamp:prior:',
		function (commentstamp, prior)
		{
            var result = new Object();
            result.scanFrom = function(reader)
            {
                reader.nextChunk();
            }
            return result;
		});
		
    // TODO process argument
	Smalltalk.ProtoObject.defineClassMethod('methodsFor:stamp:',
		function (methodsfor, stamp)
		{
            var result = new Object();
            var self = this;

            result.scanFrom = function(reader)
            {
                var compiler = bccompiler.createCompiler(mach);
                for (var chunk = reader.nextChunk(); chunk != null && chunk != ''; chunk = reader.nextChunk())
                {
                    var method = compiler.compileMethod(chunk, self);
                    self.defineMethod(method.name, method);                    
                }
            }
 
			return result;
		});

    Smalltalk.ProtoObject.copyClassMethod('methodsFor:stamp:','methodsFor:');

    // TODO process argument
	Smalltalk.ProtoObject.defineClassMethod('instanceVariableNames:',
		function (names)
		{
		});
		
	Smalltalk.Nil.defineClassMethod('ifNil:',
		function(block)
		{
			block.execute();
		});

    function toNames(text)
	{
		var words = text.split(' ');
		var result = [];
		var l = words.length;
		
		for (var k = 0; k <  l; k++)
			if (words[k])
				result.push(words[k]);
				
		if (result.length == 0)
			return null;

		return result;
	}
	
	Smalltalk.Object.compileClassMethod_("new ^self basicNew initialize.");
	Smalltalk.Object.compileMethod_("initialize ^self.");
    
    function execute(text) {
        var compiler = bccompiler.createCompiler(mach);
        var block = compiler.compileBlock(text);
        var result = block.apply();
        return result;
    }
    
    Smalltalk.execute_ = execute;
	
    var exports = { };
	exports.Smalltalk = Smalltalk;
    exports.execute = execute;
    
    var loadpath = null;
    
    function load(filename) {
        if (loadpath && filename[0] != path.sep && filename.indexOf(':') < 0)
            filename = path.join(loadpath, filename);
            
        var content = fs.readFileSync(filename).toString();
        chreader = chunkreader.createReader(content);
        var originalloadpath = loadpath;
        
        try {
            loadpath = path.dirname(path.resolve(filename));
            chreader.process(bccompiler.createCompiler(mach));
        }
        finally {
            loadpath = originalloadpath;
        }
    }
    
	if (fs) {
		exports.load = load;
        Smalltalk.load_ = load;
    }
		
	if (typeof jQuery != 'undefined')
	{
		exports.loadFiles = function(filenames, callback)
		{
			function loadNextFile()
			{
				jQuery.get(filenames[0], null, function(data) {
					var chreader = chunkreader.createReader(data);
					chreader.process(bccompiler.createCompiler(mach));
					filenames.shift();
					
					if (filenames.length > 0)
						loadNextFile();
					else if (callback)
						callback();
				}, 'text');
			}
			
			loadNextFile();
		}
	}
    
    return exports;
})();

if (typeof module !== 'undefined' && module && module.exports)
    module.exports = ajtalk;
    return ajtalk;

})();


// http://coffeescript.org/documentation/docs/browser.html

function runScripts() {
    var scripts = window.document.getElementsByTagName('script');
    
    for (var i = 0; i < scripts.length; i++) {
        var script = scripts[i];

        if (script.type != 'text/smalltalk')
            continue;
        
        var result = ajtalk.execute(script.innerHTML);
    }
}

if (window.addEventListener)
    window.addEventListener('DOMContentLoaded', runScripts, false);
else
    window.attachEvent('onload', runScripts);