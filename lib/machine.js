
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
        NativeDo: 104,
		IsNative: 105,
		IsNativeClass: 106,
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
						
						if (!fn && selector === 'compileMethod_' && isNativeClass(target)) {
							stack.push(machineResult.compileInstanceMethodForNativeClass(target, args[0]));
							break;
						}
                        
                        if (!fn && isNative(target)) {
                            var p = selector.indexOf("_");
                            
                            if (p > 0)
                                fn = target[selector.substring(0, p)];
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

				case ByteCodes.IsNative:
					var value = stack.pop();
					
					stack.push(isNative(value));
					
					break;

				case ByteCodes.IsNativeClass:
					var value = stack.pop();
					
					stack.push(isNativeClass(value));
					
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
    
    var machineResult = {
        ByteCodes: ByteCodes,
        createMachine: function (Smalltalk) { return new Machine(Smalltalk); }
    }
	
	return machineResult;
	
	function isNative(obj) {
		return obj == null || !obj.klass;
	}
	
	function isNativeClass(obj) {
		if (obj == null)
			return false;
		
		if (!isNative(obj) || !obj.prototype)
			return false;
		
		return obj === String || obj === Number || obj === Date || obj === Function || obj === Array;
	}
})();

if (typeof module !== 'undefined' && module && module.exports)
    module.exports = machine;
