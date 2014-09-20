
var ajtalk = require('../lib/ajtalk');
var Smalltalk = ajtalk.Smalltalk;
var path = require('path');

exports['Load relative file'] = function (test) {
    var filename = path.join(__dirname, 'files', 'Load.st');
    ajtalk.load(filename);
    
    test.ok(ajtalk.Smalltalk.HtmlHelloPage);
}

exports['Load Squeak Object'] = function(test) {
	ajtalk.load(__dirname + '/../squeak/Object.st');

	test.ok(Smalltalk.Object);
};

exports['Load Pharo Point'] = function(test) {
	ajtalk.load(__dirname + '/../pharo/Point.st');

	test.ok(Smalltalk.Point);
};

exports['Load Pharo Rectangle'] = function(test) {
	ajtalk.load(__dirname + '/../pharo/Rectangle.st');

	test.ok(Smalltalk.Point);
};

exports['Load Pharo Kernel Objects'] = function(test) {
	ajtalk.load(__dirname + '/../pharo/KernelObjects.st');

	test.ok(Smalltalk.ProtoObject);
	test.ok(Smalltalk.Object);
	test.ok(Smalltalk.Boolean);
};
