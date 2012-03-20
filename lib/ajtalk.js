
(function(exports) {
	var Smalltalk = {};
    
    // Experimental
    
    function ProtoObjectClass()
    {
    }
    
    function ProtoObject()
    {
        this.klass = ProtoObject;
    }
    
    ProtoObject.metaklass = ProtoObjectClass;
    ProtoObjectClass.klass = ProtoObject;
        
    ProtoObjectClass.prototype.defineMethod = function (name, method)
    {
        this.klass.prototype[name] = method;
    };
    
    ProtoObject.basicNew = function()
    {
        var obj = new ProtoObject();
        obj.klass = this;
        return obj;
    };
    
    ProtoObject.defineMethod = function (name, method)
    {
        this.prototype[name] = method;
    };
    
    function ProtoClass(name, superklass, instvarnames, clsvarnames) 
    {		
		// Function 'class' for this class objects (see basicNew)
        var func = new Function();

		if (superklass) 
			func.prototype.__proto__ = superklass.func.prototype;
			
		this.func = func;		
		this.superklass = superklass;
		this.instvarnames = instvarnames;
		this.clsvarnames = clsvarnames;
		this.name = name;
		
		if (instvarnames)
			for (var n in instvarnames)
				this.func.prototype['$' + instvarnames[n]] = null;
		
		// TODO to review if needed
		if (name)
			Smalltalk[name] = this;
    };
    
    ProtoClass.prototype.__proto__ = Function.prototype;
    ProtoClass.prototype.defineMethod = function(name, method)
    {
		if (typeof method == "function")
			this.func.prototype[name] = method;
		else
		{
			var normalized = name.replace(/:/g, '_');
			this.func.prototype[normalized] = method.toFunction();
		}
    };
    
    ProtoClass.prototype.basicNew = function()
    {
        var obj = new this.func;
        obj.klass = this;
        return obj;
    };
    
    ProtoClass.prototype.defineSubclass = function(name, instvarnames, clsvarnames)
    {
		return new ProtoClass(name, this, instvarnames, clsvarnames);
    };
	
	ProtoClass.prototype.getObjectSize = function()
	{
		var size = this.instvarnames ? this.instvarnames.length : 0;
		
		if (this.superklass)
			size += this.superklass.getObjectSize();
			
		return size;
	};
    
    // Implementation
	
	function BaseObject(klass) {
		this.klass = klass;
		this.size = klass ? klass.getInstanceSize() : 0;
		this.variables = new Array(this.size);
	};
	
	BaseObject.prototype.lookup = function(selector)
	{
		return this.klass.lookupInstanceMethod(selector);
	};
	
	BaseObject.prototype.sendMessage = function(selector, args)
	{
		var method = this.lookup(selector);
		return method.apply(this, args);
	};
	
	function BaseClass(name, instvarnames, clsvarnames, supercls) {
		this.name = name;
		this.instvarnames = instvarnames;
		this.clsvarnames = clsvarnames;
		this.supercls = supercls;
		this.methods = {};
	};
	
	BaseClass.prototype.__proto__ = BaseObject.prototype;
	
	BaseClass.prototype.defineMethod = function (selector, method) 
	{
		this.methods[selector] = method;
	};
	
	BaseClass.prototype.getInstanceSize = function() {
		var result = this.instvarnames.length;
		if (this.supercls)
			result += this.supercls.getInstanceSize();
			
		return result;
	};

	BaseClass.prototype.lookupInstanceMethod = function (selector) 
	{
		var result = this.methods[selector];
		if (result == null && this.supercls)
			return this.supercls.lookupInstanceMethod(selector);
		return result;
	};

	// TODO Quick Hack, for Object class that has no superclass
	BaseClass.prototype.lookup = function(selector)
	{
		return this.lookupInstanceMethod(selector);
	}
	
	Smalltalk.Object = new BaseClass('Object', [], [], null);

	// TODO Quick hack, class method as instance method
	Smalltalk.Object.defineMethod('compileMethod:', function(text)
		{
			var compiler = new Compiler();
			var method = compiler.compileMethod(text, this);
			this.defineMethod(method.name, method);
			return method;
		});

		// TODO Quick hack, class method as instance method
	Smalltalk.Object.defineMethod('new', function()
		{
			return new BaseObject(this);
		});
	
	var ByteCodes = {
		GetValue: 0,
		GetArgument: 1,
		GetLocal: 2,
		GetInstanceVariable: 3,
		GetGlobalVariable: 4,
		GetSelf: 5,
		SetLocal: 10,
		SetInstanceVariable: 11,
		SetGlobalVariable: 12,
		Add: 20,
		Subtract: 21,
		Multiply: 22,
		Divide: 23,
		SendMessage: 40,
		Return: 50
	};
	
	var Functions = [];
	
	function Block(arity, nlocals)
	{
		this.arity = arity;
		this.nlocals = nlocals;
		this.bytecodes = [];
		this.values = [];
	};
	
	Block.prototype.apply = function(self, args) 
	{
		return (new ExecutionBlock(this, self, args)).execute();
	};
	
	Block.prototype.compileByteCode = function(bytecode, param)
	{
		this.bytecodes.push(bytecode);
		if (param != null)
			this.bytecodes.push(param);
	};
	
	Block.prototype.toFunction = function()
	{
		var block = this;
		return function() { return block.apply(this, arguments); }
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
	
	function ExecutionBlock(block, self, args)
	{
		this.block = block;
		this.self = self;
		this.args = args;
		this.locals = new Array(block.nlocals);
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
				case ByteCodes.GetArgument:
					var na = bc[ip++];
					stack.push(this.args[na]);
					break;
				case ByteCodes.GetLocal:
					var nl = bc[ip++];
					stack.push(this.locals[nl]);
					break;
				case ByteCodes.GetInstanceVariable:
					var niv = bc[ip++];
					stack.push(this.self.variables[niv]);
					break;
				case ByteCodes.GetGlobalVariable:
					nv = bc[ip++];
					stack.push(Smalltalk[this.block.values[nv]]);
					break;
				case ByteCodes.SetInstanceVariable:
					niv = bc[ip++];
					this.self.variables[niv] = stack.pop();
					break;
				case ByteCodes.SetGlobalVariable:
					nv = bc[ip++];
					Smalltalk[this.block.values[nv]] = stack.pop();
					break;
				case ByteCodes.GetSelf:
					stack.push(this.self);
					break;
				case ByteCodes.Add:
					var op2 = stack.pop();
					var op1 = stack.pop();
					stack.push(op1 + op2);
					break;
				case ByteCodes.Subtract:
					var op2 = stack.pop();
					var op1 = stack.pop();
					stack.push(op1 - op2);
					break;
				case ByteCodes.Multiply:
					var op2 = stack.pop();
					var op1 = stack.pop();
					stack.push(op1 * op2);
					break;
				case ByteCodes.Divide:
					var op2 = stack.pop();
					var op1 = stack.pop();
					stack.push(op1 / op2);
					break;
				case ByteCodes.Return:
					return stack.pop();
					break;
				case ByteCodes.SendMessage:
					var arity = bc[ip++];
					var selector = stack.pop();
					var args = [];
					
					for (var k = 0; k < arity; k++)
						args.unshift(stack.pop());
						
					var target = stack.pop();
					stack.push(target.sendMessage(selector, args));
					
					break;
				default:
					throw "Invalid ByteCode " + bytecode
					breakl
			};
		}
		
		if (stack.length > 0)
			return stack.pop();
	}

	// Lexer
	
	var operators = [':', '=', '==', ':=', '+', '-', '*', '/', '^'];
	var binaryoperators = ['=', '==', '+', '-', '*', '/'];
	
	function Lexer(text) 
	{
		var position = 0;
		var length = text.length;
		var lasttoken = null;
		
		this.nextToken = function() {
			if (lasttoken != null) {
				var value = lasttoken;
				lasttoken = null;
				return value;
			}
		
			skipSpacesAndComments();
			
			var char = nextChar();
			
			if (char == null)
				return null;
			
			if (isLetter(char))
				return nextName(char);
				
			if (isDigit(char))
				return nextNumber(char);
				
			if (char == "'")
				return nextString();
				
			if (operators.indexOf(char) >= 0)
				return nextOperator(char);
				
			return new Token(char, TokenType.Separator);
		}
		
		this.pushToken = function(token) 
		{
			lasttoken = token;
		}
		
		function nextOperator(char)
		{
			var next = nextChar();
			
			if (next != null) 
			{
				var operator = char + next;
				
				if (operators.indexOf(operator) >= 0)
					return new Token(operator, TokenType.Operator);
				
				position--;
			}
			
			return new Token(char, TokenType.Operator);
		}
		
		function nextChar()
		{
			if (position > length)
				return null;
				
			return text[position++];
		}
		
		function skipSpacesAndComments()
		{
			while (position < length) {
				while (position < length && text[position] <= ' ')
					position++;
					
				if (position < length && text[position] == '"') 
				{
					position++;
					
					while (position < length && text[position] != '"')
						position++;
						
					if (position < length)
						position++;
						
					continue;
				}

				return;
			}
		}
		
		function nextName(char)
		{
			var name = char;
			
			while ((char = nextChar()) != null && (isLetter(char) || isDigit(char)) || char === ":") 
			{
				name += char;
				
				if (char == ':')
					break;
			}
			
			if (char == ':')
				return new Token(name, TokenType.Keyword);
				
			if (char != null)
				position--;						
				
			return new Token(name, TokenType.Name);
		}
		
		function nextString()
		{
			var string = '';
			
			while ((char = nextChar()) != null && char != "'")
				string += char;
				
			return new Token(string, TokenType.String);
		}
		
		function nextNumber(char)
		{
			var number = char;
			
			while ((char = nextChar()) != null && isDigit(char))
				number += char;
				
			if (char == '.') 
			{
				var next = nextChar();
				position--;
				
				if (next != null && isDigit(next)) 
				{
					return nextFloatNumber(number+'.');
				}
			}
				
			if (char != null)
				position--;
				
			return new Token(parseInt(number), TokenType.Number);
		}
		
		function nextFloatNumber(number)
		{
			var char;
			
			while ((char = nextChar()) != null && isDigit(char))
				number += char;
				
			if (char != null)
				position--;
				
			return new Token(parseFloat(number), TokenType.Number);
		}
		
		function isLetter(char)
		{
			if (char >= 'a' && char <= 'z')
				return true;
				
			if (char >= 'A' && char <= 'Z')
				return true;
				
			return false;
		}
		
		function isDigit(char)
		{
			if (char >= '0' && char <= '9')
				return true;
				
			return false;
		}
	}
	
	function Token(value, type)
	{
		this.value = value;
		this.type = type;
		
		this.isName = function() { return this.type == TokenType.Name; };
		this.isNumber = function() { return this.type == TokenType.Number; };
		this.isString = function() { return this.type == TokenType.String; };
		this.isOperator = function() { return this.type == TokenType.Operator; };
		this.isSeparator = function() { return this.type == TokenType.Separator; };
		this.isKeyword = function() { return this.type == TokenType.Keyword; };
		this.isSymbol = function() { return this.type == TokenType.Symbol; };
		this.isCharacter = function() { return this.type == TokenType.Character; };
		this.isBinaryOperator = function() { return this.type == TokenType.Operator && binaryoperators.indexOf(this.value) >= 0; };
	}
	
	var TokenType = { Name: 0, Number:1, String:2, Operator:3, Separator:4, Keyword:5, Symbol:6, Character:7 };
	
	function Compiler()
	{
	}
	
	Compiler.prototype.compileBlock = function(text) 
	{
		var block = new Block(0, 0);
		var lexer = new Lexer(text);
		
		this.compileExpressions(block, lexer);
		
		return block;
	};
	
	Compiler.prototype.compileMethod = function(text, klass)
	{
		var lexer = new Lexer(text);
		var signature = this.compileMethodSignature(lexer);		
		
		var method = new Block(signature.argnames.length, 0);
		method.klass = klass;
		method.argnames = signature.argnames;
		method.localnames = signature.localnames;
		method.code = text;		
		method.name = signature.name;
		
		this.compileExpressions(method, lexer);
		
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
			
		if (token.isName())
		{
			signature.name = token.value;
			return signature;
		}
				
		// TODO isBinaryOperator?
		if (token.isOperator())
		{
			signature.name = token.value;
			token = lexer.nextToken();
			if (token == null || !token.isName())
				throw "Argument name expected";
			signature.argnames.push(token.value);
			return signature;
		}
		
		while (token != null && token.isKeyword())
		{
			var keyword = token.value;
			signature.name += keyword;
			token = lexer.nextToken();
			if (token == null || !token.isName())
				throw "Argument name expected";
			signature.argnames.push(token.value);
			token = lexer.nextToken();
		}
		
		if (token != null)
			lexer.pushToken(token);
		
		return signature;
	}
	
	Compiler.prototype.compileExpressions = function(block, lexer)
	{
		var token = lexer.nextToken();
		
		while (token != null) 
		{
			lexer.pushToken(token);
			this.compileExpression(block, lexer);
			token = lexer.nextToken();
			
			if (token != null && (!token.isSeparator() || token.value != '.'))
				throw "Unexpected " + token.value;
				
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
		
		if (token == null || !token.isKeyword()) 
		{
			if (token != null)
				lexer.pushToken(token);
				
			return;
		}
		
		var selector = '';
		var arity = 0;
		
		while (token != null && token.isKeyword())
		{
			selector += token.value;
			arity++;
			this.compileBinaryExpression(block, lexer);
			token = lexer.nextToken();			
		}
		
		if (token != null)
			lexer.pushToken(token);
			
		var position = block.addValue(selector);
		block.compileByteCode(ByteCodes.GetValue, position);
		block.compileByteCode(ByteCodes.SendMessage, arity);
	}
	
	Compiler.prototype.compileBinaryExpression = function(block, lexer)
	{
		this.compileUnaryExpression(block, lexer);
		
		var token = lexer.nextToken();
		
		while (token != null && token.isBinaryOperator())
		{
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
				default:
					throw "Invalid operator " + token.value;
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
		
		while (token != null && token.isName())
		{
			var position = block.addValue(token.value);
			block.compileByteCode(ByteCodes.GetValue, position);
			block.compileByteCode(ByteCodes.SendMessage, 0);
			token = lexer.nextToken();
		}
		
		if (token != null)
			lexer.pushToken(token);
	}
	
	Compiler.prototype.compileTerm = function(block, lexer)
	{
		var token = lexer.nextToken();
		
		if (token == null)
			return;
			
		if (token.isName()) 
		{
			var name = token.value;
			var position = 0;
			var islocal = false;
			var isargument = false;
			var isvariable = false;
			
			if (block.argnames != null)
			{
				var index = block.argnames.indexOf(name);
				
				if (index >= 0)
				{
					position = index;
					isargument = true;
				}
			}
			
			if (!isargument && block.localnames != null)
			{
				index = block.localnames.indexOf(name);
				
				if (index >= 0) 
				{
					position = index;
					islocal = true;
				}
			}
			
			if (!isargument && !islocal && block.klass != null)
			{
				index = block.klass.instvarnames.indexOf(name);
				
				if (index >= 0)
				{
					position = index;
					isvariable = true;
				}
			}

			if (!isargument && !islocal && !isvariable)
				position = block.addValue(name);
			
			token = lexer.nextToken();
			
			if (isargument || token == null || !token.isOperator() || token.value != ':=')
			{
				lexer.pushToken(token);

				if (isargument)
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
		
		if (token.isSeparator() && token.value == '(')
		{
			this.compileExpression(block, lexer);
			token = lexer.nextToken();
			if (token == null || !token.isSeparator() || token.value != ')')
				throw "Expected ')'";
			return;
		}
		
		if (token.isString() || token.isNumber())
		{
			var position = block.addValue(token.value);
			block.compileByteCode(ByteCodes.GetValue, position);
			return;
		}
		
		if (token.isOperator() && token.value == '^')
		{
			this.compileExpression(block, lexer);
			block.compileByteCode(ByteCodes.Return);
			return;
		}
		
		throw "Invalid term " + token.value;
	}
	
	Compiler.prototype.compileName = function(lexer)
	{
		var token = lexer.nextToken();
		if (token == null || !token.isName())
			throw "Name expected";
		return token.value;
	}
	
	exports.BaseClass = BaseClass;
	exports.BaseObject = BaseObject;
	exports.Block = Block;
	exports.ByteCodes = ByteCodes;
	exports.Block = Block;
	
	exports.Lexer = Lexer;
	exports.Compiler = Compiler;
	
	exports.Smalltalk = Smalltalk;
    
    // Experimental
    
    exports.ProtoObject = new ProtoObject();
    exports.ProtoObjectClass = ProtoObject;
    exports.ProtoClass = ProtoClass;
	
})(typeof exports == 'undefined' ? this['ajtalk'] = {} : exports);