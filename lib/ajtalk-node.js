var ajtalk;
var Smalltalk;
var bccompiler;

if (typeof(ajtalk) === 'undefined')
    ajtalk = require('./ajtalk.js');
if (typeof(chunkreader) === 'undefined')
    chunkreader = require('./chunkreader.js');
if (typeof(bccompiler) === 'undefined')
    bccompiler = require('./bccompiler.js');

if (typeof(Smalltalk) === 'undefined')
    Smalltalk = ajtalk.Smalltalk;
    
var fs = require('fs');
    
Smalltalk.Object.subclass_instanceVariableNames_classVariableNames_('Node', '', '');

Smalltalk.Node.defineClassMethod('require:', function(filename)
{
    return require(filename);
});

Smalltalk.Node.defineClassMethod('loadst:', function(filename)
{
    var content = fs.readFileSync(filename).toString();
    var chreader = chunkreader.createReader(content);
    chreader.process(bccompiler.createCompiler());
});

