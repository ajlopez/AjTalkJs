
var ajtalk = require('..'),
    path = require('path');

exports['load SimplePoint'] = function (test) {    
    ajtalk.load(path.join(__dirname, 'files',  'SimplePoint.st'));

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

exports['load SimpleClass'] = function (test) {
    ajtalk.load(path.join(__dirname, 'files',  'SimpleClass.st'));

    test.ok(ajtalk.Smalltalk.SimpleClass);
    test.ok(ajtalk.Smalltalk.SimpleSubclass);
}

