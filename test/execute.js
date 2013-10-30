
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

exports['create native object'] = function (test) {
    var result = ajtalk.execute("NativeObject new");
    
    test.ok(result);
    test.equal(typeof result, 'object');
    test.equal(result.__proto__, Object.prototype);
    test.equal(Object.keys(result).length, 0);
}

exports['create native object using native new'] = function (test) {
    var result = ajtalk.execute("NativeObject nnew: { }");
    
    test.ok(result);
    test.equal(typeof result, 'object');
    test.equal(result.__proto__, Object.prototype);
    test.equal(Object.keys(result).length, 0);
}

exports['create native array'] = function (test) {
    var result = ajtalk.execute("NativeArray new");
    
    test.ok(result);
    test.ok(Array.isArray(result));
    test.equal(result.length, 0);
}

exports['create native array using native new'] = function (test) {
    var result = ajtalk.execute("NativeArray nnew");
    
    test.ok(result);
    test.ok(Array.isArray(result));
    test.equal(result.length, 0);
}

exports['create native array using native new with arguments'] = function (test) {
    var result = ajtalk.execute("NativeArray nnew: { 1. 2. 3 }");
    
    test.ok(result);
    test.ok(Array.isArray(result));
    test.equal(result.length, 3);
    test.equal(result[0], 1);
    test.equal(result[1], 2);
    test.equal(result[2], 3);
}

exports['block as native function'] = function (test) {
    var result = ajtalk.execute("[:a | a + 1 ] toFunction");
    
    test.ok(result);
    test.equal(typeof result, 'function');
    test.equal(result(1), 2);
}

exports['evaluate native function using value'] = function (test) {
    var result = ajtalk.execute("foo := [ 3 ] toFunction. foo value");
    
    test.ok(result);
    test.equal(result, 3);
}

exports['evaluate Smalltalk'] = function (test) {
    var result = ajtalk.execute("Smalltalk");
    
    test.ok(result);
    test.strictEqual(result, ajtalk.Smalltalk);
}

exports['evaluate equal'] = function (test) {
    var result = ajtalk.execute("1 = 1");
    
    test.ok(result);
    test.strictEqual(result, true);
}

exports['evaluate equal as false'] = function (test) {
    var result = ajtalk.execute("1 = 2");
    
    test.strictEqual(result, false);
}

exports['evaluate not equal'] = function (test) {
    var result = ajtalk.execute("1 ~= 2");
    
    test.ok(result);
    test.strictEqual(result, true);
}

exports['evaluate not equal as false'] = function (test) {
    var result = ajtalk.execute("1 ~= 1");
    
    test.strictEqual(result, false);
}

exports['evaluate nil'] = function (test) {
    var result = ajtalk.execute("nil");
    
    test.strictEqual(result, null);
}

exports['evaluate true'] = function (test) {
    var result = ajtalk.execute("true");
    
    test.ok(result);
    test.strictEqual(result, true);
}

exports['evaluate false'] = function (test) {
    var result = ajtalk.execute("false");
    
    test.strictEqual(result, false);
}

exports['evaluate ifTrue:'] = function (test) {
    var result = ajtalk.execute("true ifTrue: [ 1 ]");
    
    test.ok(result);
    test.equal(result, 1);
}

exports['evaluate ifTrue: with false'] = function (test) {
    var result = ajtalk.execute("1 = 2 ifTrue: [ 1 ]");
    
    test.strictEqual(result, null);
}

exports['evaluate ifFalse:'] = function (test) {
    var result = ajtalk.execute("false ifFalse: [ 1 ]");
    
    test.ok(result);
    test.equal(result, 1);
}

exports['evaluate ifFalse: with true'] = function (test) {
    var result = ajtalk.execute("1 = 1 ifFalse: [ 1 ]");
    
    test.strictEqual(result, null);
}

exports['evaluate ifTrue:ifFalse:'] = function (test) {
    var result = ajtalk.execute("1 = 1 ifTrue: [ 1 ] ifFalse: [0]");
    
    test.ok(result);
    test.equal(result, 1);
}

exports['evaluate ifTrue:ifFalse: with false'] = function (test) {
    var result = ajtalk.execute("1 = 2 ifTrue: [ 1 ] ifFalse: [2]");
    
    test.ok(result);
    test.equal(result, 2);
}

exports['evaluate ifFalse:IfTrue:'] = function (test) {
    var result = ajtalk.execute("1 = 2 ifFalse: [ 1 ] ifTrue: [2]");
    
    test.ok(result);
    test.equal(result, 1);
}

exports['evaluate ifFalse:IfTrue with true'] = function (test) {
    var result = ajtalk.execute("1 = 1 ifFalse: [ 1 ] ifTrue: [2]");
    
    test.ok(result);
    test.equal(result, 2);
}

exports['evaluate strict comparison'] = function (test) {
    var result = ajtalk.execute("1 == 1");
    
    test.ok(result);
    test.strictEqual(result, true);
}

exports['evaluate less comparison'] = function (test) {
    var result = ajtalk.execute("1 < 2");
    
    test.ok(result);
    test.equal(result, true);
    
    result = ajtalk.execute("1 < 1");
    
    test.strictEqual(result, false);
}

exports['evaluate greater comparison'] = function (test) {
    var result = ajtalk.execute("'b' > 'a'");
    
    test.ok(result);
    test.equal(result, true);
    
    result = ajtalk.execute("1 > 2");
    
    test.strictEqual(result, false);
}

exports['evaluate less or equal comparison'] = function (test) {
    var result = ajtalk.execute("1 <= 2");
    
    test.ok(result);
    test.equal(result, true);
    
    result = ajtalk.execute("1 <= 0");
    
    test.strictEqual(result, false);
}

exports['evaluate greater or equal comparison'] = function (test) {
    var result = ajtalk.execute("'b' >= 'a'");
    
    test.ok(result);
    test.equal(result, true);
    
    result = ajtalk.execute("1 >= 2");
    
    test.strictEqual(result, false);
}

exports['raise error using signal'] = function (test) {
    test.throws(
        function () {
            ajtalk.execute("Error signal: 'we have a problem'");
        },
        function (err) {
            if (err instanceof Error && err.message === 'we have a problem')
                return true;
        }
    );
}

