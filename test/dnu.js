
var ajtalk = require('../lib/ajtalk');
var Smalltalk = ajtalk.Smalltalk;
var path = require('path');

exports['Invoke unknow selector'] = function (test) {
    var filename = path.join(__dirname, 'files', 'SimpleDnu.st');
    ajtalk.load(filename);
    
    test.equal(ajtalk.execute('SimpleDnu new foo'), 42);
}

exports['Use Proxy'] = function (test) {
    var filename = path.join(__dirname, 'files', 'SimpleClass.st');
    ajtalk.load(filename);
    var filename = path.join(__dirname, 'files', 'SimpleProxy.st');
    ajtalk.load(filename);

    test.equal(ajtalk.execute('(SimpleProxy new: SimpleClass new) one'), 1);
    test.equal(ajtalk.execute('(SimpleProxy new: SimpleSubclass new) one'), 1);
    test.equal(ajtalk.execute('(SimpleProxy new: SimpleSubclass new) two'), 2);
}
