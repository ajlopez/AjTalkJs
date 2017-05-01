
var ajtalk = require('..');

exports['compile method for object'] = function (test) {
    var result = ajtalk.execute("a := Object new. a compileMethod: 'incr: x ^(x + 1)'. a incr: 1");
    
    test.ok(result);
	test.equal(result, 2);
}
