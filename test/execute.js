
var ajtalk = require('..');

exports['evaluate number'] = function (test) {
    var result = ajtalk.execute('1');
    
    test.ok(result);
    test.equal(result, 1);
}

exports['evaluate integer sum'] = function (test) {
    var result = ajtalk.execute('1 + 3');
    
    test.ok(result);
    test.equal(result, 4);
}

exports['evaluate real sum'] = function (test) {
    var result = ajtalk.execute('1.2 + 3.4');
    
    test.ok(result);
    test.equal(result, 1.2 + 3.4);
}

exports['execute assign'] = function (test) {
    var result = ajtalk.execute('a := 1. a');
    
    test.ok(result);
    test.equal(result, 1);
}

exports['evaluate native property'] = function (test) {
    var result = ajtalk.execute("'foo' nat: 'length'");
    
    test.ok(result);
    test.equal(result, 3);
}

exports['evaluate native method'] = function (test) {
    var result = ajtalk.execute("'foo' napply: 'toUpperCase' with: { }");
    
    test.ok(result);
    test.equal(result, 'FOO');
}
