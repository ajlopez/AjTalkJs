
var ajtalk = require('../lib/ajtalk');
var Smalltalk = ajtalk.Smalltalk;
var path = require('path');

exports['Invoke JavaScript method'] = function (test) {
    var filename = path.join(__dirname, 'files', 'SimpleJsClass.st');
    ajtalk.load(filename);
    
    test.equal(ajtalk.execute('SimpleJsClass new one'), 1);
}

exports['Invoke inc JavaScript method'] = function (test) {
    var filename = path.join(__dirname, 'files', 'SimpleJsClass.st');
    ajtalk.load(filename);
    
    test.equal(ajtalk.execute('SimpleJsClass new inc: 2'), 3);
}
