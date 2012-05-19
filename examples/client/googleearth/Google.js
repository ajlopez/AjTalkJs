
var ajtalk;
var Smalltalk;

if (typeof(ajtalk) === 'undefined')
    ajtalk = require('ajtalk.js');

if (typeof(Smalltalk) === 'undefined')
    Smalltalk = ajtalk.Smalltalk;
    
Smalltalk.Object.subclass_instanceVariableNames_classVariableNames_('Google', '', '');

Smalltalk.Google.defineClassMethod('load:with:', function(module, param)
{
    google.load(module, param);
});

Smalltalk.Google.defineClassMethod('loadCallback:', function(callback)
{
    if (callback.execute)
        google.setOnLoadCallback(function() { callback.execute(); });
    else
        google.setOnLoadCallback(callback);
});

