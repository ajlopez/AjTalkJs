
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

    Smalltalk.NativeString = String;
    String.new = function () { return new String() };
    String.new_ = function (text) { return new String(text); };

    Smalltalk.NativeDate = Date;
    Date.new = function () { return new Date() };
    Date.new_ = function (arg) { return new Date(arg); };
	// TODO other news, read https://www.w3schools.com/jsref/jsref_obj_date.asp

	// Native Function with new constructor
	// Usage: Function new: { 'a' 'b' 'return a+b;' }
    Smalltalk.NativeFunction = Function;
	// http://stackoverflow.com/questions/1606797/use-of-apply-with-new-operator-is-this-possible
	// http://stackoverflow.com/questions/3362471/how-can-i-call-a-javascript-constructor-using-call-or-apply
	function TempFunction() {};
	TempFunction.prototype = Function;
    Function.new_ = function (args) { return Function.apply(new TempFunction(), args); };
    
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
            
            protoklass.prototype.defineObjectMethod = function(obj, name, method)
            {
                var mthname = name.replace(/:/g, '_');

                if (typeof method == "function")
                    obj[mthname] = method;
                else {
                    method.class = obj.klass;
                    obj[mthname] = method.asFunction();
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
		
	Smalltalk.ProtoObject.defineMethod('compileMethod:', function(text)
		{
			var compiler = bccompiler.createCompiler(mach);
			var method = compiler.compileMethod(text, this.klass);
			this.klass.defineObjectMethod(this, method.name, method);
			return method;
		});
		
	machine.compileInstanceMethodForNativeClass = function(klass, text) {
		var compiler = bccompiler.createCompiler(mach);
		var method = compiler.compileMethod(text, this);
		var fn = method.asFunction();
        var mthname = method.name.replace(/:/g, '_');
		klass.prototype[mthname] = fn;
		return fn;
	}
	
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
	
	Smalltalk.JavaScript = {};
    
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
	
	// Is Node
	if (typeof module !== 'undefined' && module && module.exports) {
		Smalltalk.Object.subclass_instanceVariableNames_classVariableNames_('Node', '', '');

		Smalltalk.Node.defineClassMethod('require:', function(filename)
		{
			return require(filename);
		});

		Smalltalk.Node.defineClassMethod('loadst:', function(filename)
		{
			var content = fs.readFileSync(filename).toString();
			var chreader = chunkreader.createReader(content);
			chreader.process(bccompiler.createCompiler(Smalltalk.Machine));
		});
	}
    
    return exports;
})();

if (typeof module !== 'undefined' && module && module.exports)
    module.exports = ajtalk;
