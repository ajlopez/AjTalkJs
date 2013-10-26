
var http = require('http');
var ajtalk = require('../..');

ajtalk.Smalltalk.require_ = function (name) {
    return require(name);
}

ajtalk.load('WebServer.st');
