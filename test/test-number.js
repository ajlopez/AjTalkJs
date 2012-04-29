
var ajtalk = require('../lib/ajtalk');
var Smalltalk = ajtalk.Smalltalk;

exports['Number methods'] = function(test) {
	var one = new Number(1);
	var two = new Number(2);
	test.equal(1, one.min_(2));
	test.equal(2, one.max_(2));
	test.equal(1, one.min_(two));
	test.equal(2, one.max_(two));
	test.equal(6, 3 * 2);
	test.equal(6, 3 * two);
	test.equal(6, two * 3);
	test.equal(6, two['*'](3));
	test.ok(two['@']);
	test.ok((two+1)['@']);
	test.ok(two.isInteger());
	test.ok(!Math.PI.isInteger());
	var onehalf = new Number(1.5);
	test.equal(1, one.rounded());
	test.equal(2, onehalf.rounded());
	var minusonehalf = new Number(-1.5);
	test.equal(-1, minusonehalf.rounded());
	
	test.done();
}