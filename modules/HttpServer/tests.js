
var ajtalk = require('../..'),
    assert = require('assert');

ajtalk.load('HttpServer.st');

assert.ok(ajtalk.Smalltalk.HttpServer);

var server = ajtalk.Smalltalk.HttpServer.new();

assert.ok(server);
assert.ok(server.$server);
