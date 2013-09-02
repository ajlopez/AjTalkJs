
var context = require('../lib/context');

exports['context without instance variable'] = function (test) {
    var mycontext = context.createContext();
    test.equal(mycontext.isInstanceVariable('a'), false);
}

exports['context with instance variable'] = function (test) {
    var mycontext = context.createContext();
    mycontext.defineInstanceVariable('a');
    test.equal(mycontext.isInstanceVariable('a'), true);
}