
var ajtalk = require('../lib/ajtalk');
var Smalltalk = ajtalk.Smalltalk;
var path = require('path');

exports['Invoke unknow selector'] = function (test) {
    var filename = path.join(__dirname, 'files', 'SimpleDnu.st');
    ajtalk.load(filename);
    
    test.equal(ajtalk.execute('SimpleDnu new foo'), 42);
}
