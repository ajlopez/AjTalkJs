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
				case 'isNative':
					block.compileByteCode(ByteCodes.IsNative);
					return;
				case 'isNativeClass':
					block.compileByteCode(ByteCodes.IsNativeClass);
					return;
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
