
var ajtalk = require('ajtalk');
ajtalk.Smalltalk.require_ = function (name) { return require(name); };

ajtalk.load('app.st');

