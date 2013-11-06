
var ajtalk = require('..');

exports['require local module using Init.js'] = function (test) {
    var cwd = process.cwd();
    try {
        process.chdir(__dirname);
        ajtalk.execute("Smalltalk require: 'counter'");
        var result = ajtalk.Smalltalk.Counter;
        test.equal(result, 1);
    }
    finally {
        process.chdir(cwd);
    }
}