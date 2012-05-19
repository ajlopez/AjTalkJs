
var ajtalk;
var Smalltalk;

if (typeof(ajtalk) === 'undefined')
    ajtalk = require('ajtalk.js');

if (typeof(Smalltalk) === 'undefined')
    Smalltalk = ajtalk.Smalltalk;
    
Smalltalk.Object.subclass_instanceVariableNames_classVariableNames_('GoogleEarth', '', '');

Smalltalk.GoogleEarth.defineClassMethod('initialize', function()
{
    Smalltalk.Google.load_with_("earth", "1");
    return this;
});

Smalltalk.GoogleEarth.defineClassMethod('createInstance:callback:', function(name, callback)
{
    var cb = callback;
    
    if (callback.executeWithArguments)
        cb = function(instance) { callback.executeWithArguments([instance]); };
        
    var newinstance = this.basicNew();
        
    google.earth.createInstance(name, function(instance) { newinstance.$instance = instance; cb(newinstance); }, function(errorCode) { alert(errorCode); });
    
    return newinstance;
});

Smalltalk.GoogleEarth.defineMethod('show', function()
{
    this.$instance.getWindow().setVisibility(true);
    return this;
});

