
var ajtalk = require('../../..'),
    fs = require('fs');

function executeFile(filename) {
    var text = fs.readFileSync(filename).toString();
	var result = ajtalk.execute(text);
};

process.argv.forEach(function(val) {
    if (val.slice(-3) == ".st")
        ajtalk.load(val);
});

