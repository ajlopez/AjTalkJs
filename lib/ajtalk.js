
(function(exports) {
	function BaseObject(klass, size) {
		this.$klass = klass;
		this.$variables = [];

		for (var k = 0; k < size; k++)
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
	
	function BaseClass() {
		this.$methods = {};
	};
	
	BaseClass.prototype.__proto__ = BaseObject.prototype;
	
	BaseClass.prototype.defineMethod = function (selector, method) 
	{
		this.$methods[selector] = method;
	};

	BaseClass.prototype.lookupInstanceMethod = function (selector) 
	{
		return this.$methods[selector];
	};
	
	exports.BaseClass = BaseClass;
	exports.BaseObject = BaseObject;
	
})(exports == undefined ? this['ajtalk'] = {} : exports);