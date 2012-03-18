
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
		SetLocal: 10,
		SetInstanceVariable: 11,
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
				case ByteCodes.SetInstanceVariable:
					var niv = bc[ip++];
					this.self.variables[niv] = stack.pop();
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
		
			skipSpaces();
			
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
				return new Token(char, TokenType.Operator);
				
			return new Token(char, TokenType.Separator);
		}
		
		this.pushToken = function(token) 
		{
			lasttoken = token;
		}
		
		function nextChar()
		{
			if (position > length)
				return null;
				
			return text[position++];
		}
		
		function skipSpaces()
		{
			while (position < length && text[position] <= ' ')
				position++;
		}
		
		function nextName(char)
		{
			var name = char;
			
			while ((char = nextChar()) != null && (isLetter(char) || isDigit(char)) || char === "-")
				name += char;
				
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
	}
	
	var TokenType = { Name: 0, Number:1, String:2, Operator:3, Separator:4 };
	
	exports.BaseClass = BaseClass;
	exports.BaseObject = BaseObject;
	exports.Block = Block;
	exports.ByteCodes = ByteCodes;
	exports.Block = Block;
	
	exports.Lexer = Lexer;
	
})(exports == undefined ? this['ajtalk'] = {} : exports);