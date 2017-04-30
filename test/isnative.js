
var ajtalk = require('..'),
    path = require('path');

exports['evaluate string isNative'] = function (test) {
    var result = ajtalk.execute("'foo' isNative");
    
    test.ok(result);
    test.strictEqual(result, true);
}

exports['evaluate integer isNative'] = function (test) {
    var result = ajtalk.execute("42 isNative");
    
    test.ok(result);
    test.strictEqual(result, true);
}

exports['evaluate array isNative'] = function (test) {
    var result = ajtalk.execute("NativeArray new isNative");
    
    test.ok(result);
    test.strictEqual(result, true);
}

exports['evaluate object isNative'] = function (test) {
    var result = ajtalk.execute("Object new isNative");
    
    test.strictEqual(result, false);
}
