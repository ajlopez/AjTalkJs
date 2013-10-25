
// New Experimental Implementation

if (typeof require != 'undefined') {
	var fs = require('fs');
    var chunkreader = require('./chunkreader');
    var machine = require('./machine');
    var lexer = require('./lexer');
}
	
var ByteCodes = machine.ByteCodes;
var hasjquery = (typeof jQuery != 'undefined');
var TokenType = lexer.TokenType;

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
	
    // Smalltalk variable

	var Smalltalk = {};
    
    if (typeof window !== 'undefined')
        Smalltalk.Global = window;
    else
        Smalltalk.Global = global;
    
    machine.defineSmalltalk(Smalltalk);
    	
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
                else
                    this.func.prototype[mthname] = method.toFunction();
            }
            
            protoklass.prototype.defineClassMethod = function(name, method)
            {
                var mthname = name.replace(/:/g, '_');
                if (typeof method == "function")
                    this.proto[mthname] = method;
                else
                    this.proto[mthname] = method.toFunction();
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
			var compiler = new Compiler();
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
			var compiler = new Compiler();
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
                var compiler = new Compiler();
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

    function isBinaryOperator(token) {
        return (token.type == TokenType.Sign && token.value[0] != ':') || (token.type == TokenType.Punctuation && (token.value==',' || token.value=='|'));    
    }

    function isNumber(token) {
        return token.type === TokenType.Integer || token.type === TokenType.Real;
    }
       
	function Compiler()
	{
	}
	
	Compiler.prototype.compileBlock = function(text) 
	{
		var block = machine.createBlock(0, 0);
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
		
		var method = machine.createBlock(signature.argnames.length, 0);
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
		if (token.type == TokenType.Sign)
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
			if (token == null || token.type !== TokenType.Punctuation || token.value !== ')')
				throw "Expected ')'";
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
            var blk = machine.createBlock(0, 0);
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
			var position = block.addValue(token.value);
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
	
	Smalltalk.Object.compileClassMethod_("new ^self basicNew.");
	
    var exports = { };
	exports.Compiler = Compiler;	
	exports.Smalltalk = Smalltalk;
	
	if (fs)
		exports.load = function(filename)
		{
			var content = fs.readFileSync(filename).toString();
			chreader = chunkreader.createReader(content);
			chreader.process(new Compiler());
		}
		
	if (typeof jQuery != 'undefined')
	{
		exports.loadFiles = function(filenames, callback)
		{
			function loadNextFile()
			{
				jQuery.get(filenames[0], null, function(data) {
					var chreader = chunkreader.createReader(data);
					chreader.process(new Compiler());
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
