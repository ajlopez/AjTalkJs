
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
				case ByteCodes.Return:
					return stack.pop();
					break;
			};
		}
	}
		
	exports.BaseClass = BaseClass;
	exports.BaseObject = BaseObject;
	exports.Block = Block;
	exports.ByteCodes = ByteCodes;
	exports.Block = Block;
	
})(exports == undefined ? this['ajtalk'] = {} : exports);