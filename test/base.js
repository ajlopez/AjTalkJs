
var ajtalk = require('../lib/ajtalk');
var Smalltalk = ajtalk.Smalltalk;

exports['Smalltalk Object'] = function(test) {
	test.ok(Smalltalk);
	test.ok(Smalltalk.ProtoObject);
	test.ok(Smalltalk.Object);
	test.ok(Smalltalk.NativeObject);
	test.ok(Smalltalk.NativeArray);
};

