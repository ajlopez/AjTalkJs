
var ajtalk = require('..'),
    path = require('path');

ajtalk.load(path.join(__dirname, 'files',  'SimplePoint.st'));

exports['load SimplePoint'] = function (test) {    
    test.ok(ajtalk.Smalltalk.SimplePoint);
}

exports['create SimplePoint'] = function (test) {
    var result = ajtalk.execute('SimplePoint new');
    
    test.ok(result);
    test.strictEqual(result.$x, 0);
    test.strictEqual(result.$y, 0);
    
    test.strictEqual(result.x(), 0);
    test.strictEqual(result.y(), 0);
}