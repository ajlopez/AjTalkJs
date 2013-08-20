
var parser = require('../lib/parser');

exports['parse and compile integer'] = function (test) {
    var myparser = parser.createParser('123');    
    
    var expression = myparser.parse();
    
    test.ok(expression);
    
    var result = expression.compile();
    
    test.ok(result);
    test.equal(result, '123');
};

exports['parse and compile integer with spaces'] = function (test) {
    var myparser = parser.createParser('  123   ');    
    
    var expression = myparser.parse();
    
    test.ok(expression);
    
    var result = expression.compile();
    
    test.ok(result);
    test.equal(result, '123');
};

exports['parse and compile string'] = function (test) {
    var myparser = parser.createParser("'foo'");    
    
    var expression = myparser.parse();
    
    test.ok(expression);
    
    var result = expression.compile();
    
    test.ok(result);
    test.equal(result, "'foo'");
};

exports['parse and compile name'] = function (test) {
    var myparser = parser.createParser("a");    
    
    var expression = myparser.parse();
    
    test.ok(expression);
    
    var result = expression.compile();
    
    test.ok(result);
    test.equal(result, "a");
};

exports['parse and compile unary message'] = function (test) {
    var myparser = parser.createParser("a b");    
    
    var expression = myparser.parse();
    
    test.ok(expression);
    
    var result = expression.compile();
    
    test.ok(result);
    test.equal(result, "a.b()");
};

exports['parse and compile unary messages'] = function (test) {
    var myparser = parser.createParser("a b c d");    
    
    var expression = myparser.parse();
    
    test.ok(expression);
    
    var result = expression.compile();
    
    test.ok(result);
    test.equal(result, "a.b().c().d()");
};

exports['parse and compile binary message'] = function (test) {
    var myparser = parser.createParser("a + c");    
    
    var expression = myparser.parse();
    
    test.ok(expression);
    
    var result = expression.compile();
    
    test.ok(result);
    test.equal(result, "(a + c)");
};

exports['parse and compile keyword message'] = function (test) {
    var myparser = parser.createParser("a add: 1");    
    
    var expression = myparser.parse();
    
    test.ok(expression);
    
    var result = expression.compile();
    
    test.ok(result);
    test.equal(result, "a.add_(1)");
};

exports['parse and compile two keywords message'] = function (test) {
    var myparser = parser.createParser("a add: 1 with: 2");    
    
    var expression = myparser.parse();
    
    test.ok(expression);
    
    var result = expression.compile();
    
    test.ok(result);
    test.equal(result, "a.add_with_(1, 2)");
};