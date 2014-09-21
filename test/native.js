
var ajtalk = require('..'),
    path = require('path');

exports['evaluate string length'] = function (test) {
    var result = ajtalk.execute("'foo' length");
    
    test.ok(result);
    test.equal(result, 3);
}

exports['evaluate string to upper case'] = function (test) {
    var result = ajtalk.execute("'foo' toUpperCase");
    
    test.ok(result);
    test.equal(result, 'FOO');
}

exports['evaluate Math'] = function (test) {
    var result = ajtalk.execute("Math");
    
    test.ok(result);
    test.strictEqual(result, Math);
}

exports['evaluate Date'] = function (test) {
    var result = ajtalk.execute("Date");
    
    test.ok(result);
    test.strictEqual(result, Date);
}
