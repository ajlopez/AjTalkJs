
var ajtalk = require('../../..'),
    fs = require('fs');

// local require
ajtalk.Smalltalk.require_ = function (name) { return require(name); };

function executeFile(filename) {
    var text = fs.readFileSync(filename).toString();
	var result = ajtalk.execute(text);
};

process.argv.forEach(function(val) {
    if (val.slice(-3) == ".st")
        executeFile(val);
});

