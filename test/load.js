
var ajtalk = require('..'),
    path = require('path');

exports['Load relative file'] = function (test) {
    var filename = path.join(__dirname, 'files', 'Load.st');
    ajtalk.load(filename);
    
    test.ok(ajtalk.Smalltalk.HtmlHelloPage);
}