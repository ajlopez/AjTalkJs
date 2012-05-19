
var ajtalk = require('../lib/ajtalk');
var Smalltalk = ajtalk.Smalltalk;
require('../lib/ajtalk-node.js');

exports['Require Javascript'] = function(test) {
    test.ok(Smalltalk.Node);
    
    Smalltalk.Node.require_(__dirname + '/files/Point.st.js');

    test.ok(Smalltalk.Point);
    var point = Smalltalk.Point.basicNew();
    test.ok(point);
	
	test.done();
};
