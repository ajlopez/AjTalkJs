
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

exports['evaluate String'] = function (test) {
    var result = ajtalk.execute("String");
    
    test.ok(result);
    test.strictEqual(result, String);
}

exports['evaluate new String'] = function (test) {
    var result = ajtalk.execute("String new: 'foo'");
    
    test.ok(result);
    test.equal(result, "foo");
}

exports['evaluate NativeString'] = function (test) {
    var result = ajtalk.execute("NativeString");
    
    test.ok(result);
    test.strictEqual(result, String);
}

exports['evaluate Number'] = function (test) {
    var result = ajtalk.execute("Number");
    
    test.ok(result);
    test.strictEqual(result, Number);
}

exports['evaluate NativeArray'] = function (test) {
    var result = ajtalk.execute("NativeArray");
    
    test.ok(result);
    test.strictEqual(result, Array);
}

exports['evaluate Function'] = function (test) {
    var result = ajtalk.execute("Function");
    
    test.ok(result);
    test.strictEqual(result, Function);
}

exports['evaluate NativeFunction'] = function (test) {
    var result = ajtalk.execute("NativeFunction");
    
    test.ok(result);
    test.strictEqual(result, Function);
}

exports['create and evaluate Function'] = function (test) {
    var result = ajtalk.execute("Function new: { 'a' 'b' 'return a+b;' }");
    
    test.ok(result);
    test.equal(result(1, 2), 3);
}

exports['evaluate NativeObject'] = function (test) {
    var result = ajtalk.execute("NativeObject");
    
    test.ok(result);
    test.strictEqual(result, Object);
}

exports['create NativeObject'] = function (test) {
    var result = ajtalk.execute("NativeObject new");
    
    test.ok(result);
	test.equal(typeof result, 'object');
}


