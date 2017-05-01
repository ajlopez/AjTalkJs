
var ajtalk = require('..'),
    path = require('path');

exports['evaluate String isNativeClass'] = function (test) {
    var result = ajtalk.execute("String isNativeClass");
    
    test.ok(result);
    test.strictEqual(result, true);
}

exports['evaluate Date isNativeClass'] = function (test) {
    var result = ajtalk.execute("Date isNativeClass");
    
    test.ok(result);
    test.strictEqual(result, true);
}

exports['evaluate Number isNativeClass'] = function (test) {
    var result = ajtalk.execute("Number isNativeClass");
    
    test.ok(result);
    test.strictEqual(result, true);
}

exports['evaluate Function isNativeClass'] = function (test) {
    var result = ajtalk.execute("Function isNativeClass");
    
    test.ok(result);
    test.strictEqual(result, true);
}

exports['evaluate function isNativeClass'] = function (test) {
    var result = ajtalk.execute("(Function new: { 'a' 'b' 'return a+b;' }) isNativeClass");
    
    test.strictEqual(result, false);
}

exports['evaluate NativeArray isNativeClass'] = function (test) {
    var result = ajtalk.execute("NativeArray isNativeClass");
    
    test.ok(result);
    test.strictEqual(result, true);
}
