
(function(exports) {
	function BaseObject(klass) {
		this.$klass = klass;
		this.$variables = [];
		this.$size = klass ? klass.getInstanceSize() : 0;

		for (var k = 0; k < this.$size; k++)
			this.$variables.push(null);
	};
	
	BaseObject.prototype.lookup = function(selector)
	{
		return this.$klass.lookupInstanceMethod(selector);
	}
	
	BaseObject.prototype.sendMessage = function(selector, args)
	{
		var method = this.lookup(selector);
		method.apply(this, args);
	}
	
	function BaseClass(name, instvarnames, clsvarnames, supercls) {
		this.$name = name;
		this.$instvarnames = instvarnames;
		this.$clsvarnames = clsvarnames;
		this.$supercls = supercls;
		this.$methods = {};
	};
	
	BaseClass.prototype.__proto__ = BaseObject.prototype;
	
	BaseClass.prototype.defineMethod = function (selector, method) 
	{
		this.$methods[selector] = method;
	};
	
	BaseClass.prototype.getInstanceSize = function() {
		var result = this.$instvarnames.length;
		if (this.$supercls)
			result += this.$supercls.getInstanceSize();
			
		return result;
	};

	BaseClass.prototype.lookupInstanceMethod = function (selector) 
	{
		return this.$methods[selector];
	};
	
	exports.BaseClass = BaseClass;
	exports.BaseObject = BaseObject;
	
})(exports == undefined ? this['ajtalk'] = {} : exports);