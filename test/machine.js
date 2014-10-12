
var machine = require('../lib/machine');
var ByteCodes = machine.ByteCodes;

exports['machine bytecodes'] = function (test) {
    test.ok(machine.ByteCodes);
    test.equal(typeof machine.ByteCodes, 'object');
}

exports['create block'] = function (test) {
    var result = machine.createMachine().createBlock();
    test.ok(result);
}

exports['add value'] = function (test) {
    var block = machine.createMachine().createBlock();
    var result = block.addValue(1);
    test.equal(result, 0);
}

exports['add two values'] = function (test) {
    var block = machine.createMachine().createBlock();
    var result = block.addValue(1);
    test.equal(result, 0);
    var result = block.addValue(2);
    test.equal(result, 1);
}

exports['add value twice'] = function (test) {
    var block = machine.createMachine().createBlock();
    var result = block.addValue(1);
    test.equal(result, 0);
    var result = block.addValue(1);
    test.equal(result, 0);
}

exports['return value'] = function (test) {
    var block = machine.createMachine().createBlock();
    var position = block.addValue(1);
    block.compileByteCode(ByteCodes.GetValue, position);
    block.compileByteCode(ByteCodes.Return);
    var result = block.asFunction().apply(null);
    test.ok(result);
    test.equal(result, 1);
}

exports['return first argument'] = function (test) {
    var block = machine.createMachine().createBlock();
    block.compileByteCode(ByteCodes.GetArgument, 0);
    block.compileByteCode(ByteCodes.Return);
    var result = block.asFunction().apply(null, [10, 12]);
    test.ok(result);
    test.equal(result, 10);
}

exports['return second argument'] = function (test) {
    var block = machine.createMachine().createBlock();
    block.compileByteCode(ByteCodes.GetArgument, 1);
    block.compileByteCode(ByteCodes.Return);
    var result = block.asFunction().apply(null, [10, 12]);
    test.ok(result);
    test.equal(result, 12);
}

