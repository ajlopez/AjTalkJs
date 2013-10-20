
var machine = require('../lib/machine');

exports['machine bytecodes'] = function (test) {
    test.ok(machine.ByteCodes);
    test.equal(typeof machine.ByteCodes, 'object');
}