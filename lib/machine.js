
var machine = (function() {
	var ByteCodes = {
		GetValue: 0,
		GetArgument: 1,
		GetLocal: 2,
		GetInstanceVariable: 3,
		GetGlobalVariable: 4,
		GetSelf: 5,
		GetNull: 6,
		GetBlock: 7,
		
		SetLocal: 10,
		SetInstanceVariable: 11,
		SetGlobalVariable: 12,
		
		Add: 20,
		Subtract: 21,
		Multiply: 22,
		Divide: 23,
		Concatenate: 24,
		
		Primitive: 30,
		NativePrimitive: 31,
		LastTarget: 32,
		
		SendMessage: 40,
		NewList: 41,
		Return: 50,
		
		// Javascript bytecodes
		
		NativeAt: 100,
		NativeAtPut: 101,
		NativeApply: 102,
		NativeNew: 103
	};
    	
	function Block(arity, nlocals)
	{
        arity = arity || 0;
        nlocals = nlocals || 0;
        
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
		var result = function() { return block.apply(this, arguments); }
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
	
	function ExecutionBlock(block, self, args)
	{
		this.block = block;
		this.self = self;
		this.args = args;
		this.locals = new Array(block.nlocals);
	}
	
	ExecutionBlock.prototype.executeWithParameters = function(parameters)
	{
		this.parameters = parameters;
		return this.execute();
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
					stack.push(new ExecutionBlock(this.block.values[nv], this.self, null));
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
					stack.push(Smalltalk[this.block.values[nv]]);
					break;
				case ByteCodes.SetInstanceVariable:
					niv = bc[ip++];
					this.self[this.block.values[niv]] = stack.pop();
					break;
				case ByteCodes.SetGlobalVariable:
					nv = bc[ip++];
					Smalltalk[this.block.values[nv]] = stack.pop();
					break;
				case ByteCodes.GetSelf:
					stack.push(this.self);
					break;
				case ByteCodes.GetNull:
					stack.push(null);
					break;
				case ByteCodes.LastTarget:
					stack.push(target);
					break;
				case ByteCodes.Concatenate:
					var op2 = stack.pop();
					var op1 = stack.pop();
					stack.push(op1.toString() + op2.toString());
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
				case ByteCodes.Primitive:
					var nprim = bc[ip++];
					return Primitives[nprim](this);
					break;
				case ByteCodes.NativePrimitive:
					var nv = bc[ip++];
					var natprim = this.block.values[nv];
					stack.push(eval(natprim));
					break;
				case ByteCodes.SendMessage:
					var arity = bc[ip++];
					var selector = stack.pop();
					var args = [];
					
					for (var k = 0; k < arity; k++)
						args.unshift(stack.pop());
						
					var target = stack.pop();
					
					if (!target)
						target = Smalltalk.Nil;
                    
					stack.push(target[selector].apply(target, args));
					
					break;
				case ByteCodes.NativeNew:
					var args = stack.pop();
					var target = stack.pop();
					
					stack.push(target.prototype.constructor.apply(target, args));
					
					break;
				case ByteCodes.NativeApply:
					var args = stack.pop();
					var selector = stack.pop();						
					var target = stack.pop();
					
					stack.push(target[selector].apply(target, args));
					
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
    
    function createBlock(arity, nlocals) {
        return new Block(arity, nlocals);
    }
    
    return {
        ByteCodes: ByteCodes,
        createBlock: createBlock
    }
})();

if (typeof module !== 'undefined' && module && module.exports)
    module.exports = machine;
