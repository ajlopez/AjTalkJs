
var context = require('../lib/context');

exports['context without local variable'] = function (test) {
    var mycontext = context.createContext();
    test.equal(mycontext.isLocal('a'), false);
}

exports['context with local variable'] = function (test) {
    var mycontext = context.createContext();
    mycontext.defineLocal('a');
    test.equal(mycontext.isLocal('a'), true);
}