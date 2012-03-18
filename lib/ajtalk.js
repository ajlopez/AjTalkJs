
(function(exports) {
	function BaseObject(klass) {
		this.klass = klass;
		this.size = klass ? klass.getInstanceSize() : 0;
		this.variables = new Array(this.size);
	};
	
	BaseObject.prototype.lookup = function(selector)
	{
		return this.klass.lookupInstanceMethod(selector);
	}
	
	BaseObject.prototype.sendMessage = function(selector, args)
	{
		var method = this.lookup(selector);
		return method.apply(this, args);
	}
	
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
		return this.methods[selector];
	};
	
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
			};
		}
	}

	// Lexer
	
	var operators = [':', '=', '==', ':=', '+', '-', '*', '/', '^'];
	
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
				return nextFloatNumber(number+'.');
				
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
	}
	
	var TokenType = { Name: 0, Number:1, String:2, Operator:3, Separator:4, Keyword:5, Symbol:6, Character:7 };
	
	function Compiler()
	{
	}
	
	Compiler.prototype.compileBlock = function(text) 
	{
		var block = new Block(0, 0);
		var lexer = new Lexer(text);
		
		this.compileExpression(block, lexer);
		
		return block;
	};
	
	Compiler.prototype.compileExpression = function(block, lexer)
	{
		return this.compileTerm(block, lexer);
	}
	
	Compiler.prototype.compileTerm = function(block, lexer)
	{
		var token = lexer.nextToken();
		
		if (token == null)
			return;
			
		if (token.isName()) 
		{
			var name = token.value;
			var position = block.addValue(name);
			token = lexer.nextToken();
			
			if (token == null || !token.isOperator() || token.value != ':=')
			{
				lexer.pushToken(token);
				block.compileByteCode(ByteCodes.GetGlobalVariable, position);
				return;
			}
			
			this.compileExpression(block, lexer);
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
		
		throw "Invalid term " + token.value;
	}
	
	Compiler.prototype.compileName = function(lexer)
	{
		var token = lexer.nextToken();
		if (token == null || !token.isName())
			throw "Name expected";
		return token.value;
	}
	
	var Smalltalk = {};
	
	exports.BaseClass = BaseClass;
	exports.BaseObject = BaseObject;
	exports.Block = Block;
	exports.ByteCodes = ByteCodes;
	exports.Block = Block;
	
	exports.Lexer = Lexer;
	exports.Compiler = Compiler;
	
	exports.Smalltalk = Smalltalk;
	
})(exports == undefined ? this['ajtalk'] = {} : exports);