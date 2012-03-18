
(function(exports) {
	function BaseObject(klass, size) {
		this.$klass = klass;
		this.$variables = [];

		for (var k = 0; k < size; k++)
			this.$variables.push(null);
	}
	
	function BaseClass() {
		this.$methods = {};
	}
	
	BaseClass.prototype.__proto__ = BaseObject.prototype;
	
	exports.BaseClass = BaseClass;
	exports.BaseObject = BaseObject;
	
})(exports == undefined ? this['ajtalk'] = {} : exports);