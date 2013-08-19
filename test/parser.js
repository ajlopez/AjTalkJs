
var parser = require('../lib/parser');

exports['parse and compile integer'] = function (test) {
    var myparser = parser.createParser('123');    
    
    var expression = myparser.parse();
    
    test.ok(expression);
    
    var result = expression.compile();
    
    test.ok(result);
    test.equal(result, '123');
};