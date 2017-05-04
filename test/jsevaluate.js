
var ajtalk = require('..');

exports['evaluate number'] = function (test) {
    var result = ajtalk.execute("JavaScript evaluate: '1'");
    
    test.ok(result);
    test.equal(result, 1);
}
