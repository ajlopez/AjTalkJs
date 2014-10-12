
var ajtalk = require('../../..'),
    fs = require('fs');

// local require
ajtalk.Smalltalk.require_ = function (name) { return require(name); };

process.argv.forEach(function(val) {
    if (val.slice(-3) == ".st")
        ajtalk.load(val);
});

