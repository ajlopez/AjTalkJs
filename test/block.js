
var ajtalk = require('../lib/ajtalk');
var Smalltalk = ajtalk.Smalltalk;
var path = require('path');

exports['Evaluate block with value'] = function (test) {
    test.equal(ajtalk.execute('[ 42 ] value'), 42);
}

exports['Evaluate block with value:'] = function (test) {
    test.equal(ajtalk.execute('[ :a | a + 1 ] value: 41'), 42);
}
