
var parser = require('../lib/parser'),
    context = require('../lib/context');

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

exports['parse and compile unary and keyword message'] = function (test) {
    var myparser = parser.createParser("a increment add: 1 with: 2");    
    
    var expression = myparser.parse();
    
    test.ok(expression);
    
    var result = expression.compile();
    
    test.ok(result);
    test.equal(result, "a.increment().add_with_(1, 2)");
};

exports['parse and compile unary and keyword messages'] = function (test) {
    var myparser = parser.createParser("a increment add: b decrement with: 2");    
    
    var expression = myparser.parse();
    
    test.ok(expression);
    
    var result = expression.compile();
    
    test.ok(result);
    test.equal(result, "a.increment().add_with_(b.decrement(), 2)");
};

exports['parse and compile instance variable name'] = function (test) {
    var myparser = parser.createParser("a");
    var mycontext = context.createContext();
    
    mycontext.defineInstanceVariable('a');
    
    var expression = myparser.parse(mycontext);
    
    test.ok(expression);
    
    var result = expression.compile();
    
    test.ok(result);
    test.equal(result, "self.$a");
};

exports['parse local variables'] = function (test) {
    var myparser = parser.createParser("| a b |");
    var mycontext = context.createContext();
    
    var expression = myparser.parse(mycontext);
    
    test.ok(expression);
    
    var result = expression.compile();
    
    test.ok(result);
    test.ok(result.indexOf('var a;') >= 0);
    test.ok(result.indexOf('var b;') >= 0);
    test.ok(mycontext.isLocalVariable('a'));
    test.ok(mycontext.isLocalVariable('b'));
};

exports['parse simple assignment'] = function (test) {
    var myparser = parser.createParser("a := 1");
    var mycontext = context.createContext();
    mycontext.defineLocalVariable('a');
    
    var expression = myparser.parse(mycontext);
    
    test.ok(expression);
    
    var result = expression.compile();
    
    test.ok(result);
    test.equal(result, 'a = 1;');
};

exports['parse global variable'] = function (test) {
    var myparser = parser.createParser("Transcript");
    var mycontext = context.createContext();
    
    var expression = myparser.parse(mycontext);
    
    test.ok(expression);
    
    var result = expression.compile();
    
    test.ok(result);
    test.equal(result, 'Smalltalk.Transcript');
};
