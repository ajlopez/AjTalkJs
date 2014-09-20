
var ajtalk = require('../lib/ajtalk');
var Smalltalk = ajtalk.Smalltalk;
require('../lib/ajtalk-node.js');

exports['Require Javascript File'] = function(test) {
    test.ok(Smalltalk.Node);
    
    Smalltalk.Node.require_(__dirname + '/files/Point.st.js');

    test.ok(Smalltalk.Point);
    var point = Smalltalk.Point.basicNew();
    test.ok(point);
};

exports['Load st File'] = function(test) {
    test.ok(Smalltalk.Node);
    
    Smalltalk.Node.loadst_(__dirname + '/files/HtmlHelloPage.st');

    test.ok(Smalltalk.HtmlHelloPage);
};
