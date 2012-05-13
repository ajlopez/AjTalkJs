var ajtalk;
var Smalltalk;

if (typeof(ajtalk) === 'undefined')
    ajtalk = require('ajtalk.js');

if (typeof(Smalltalk) === 'undefined')
    Smalltalk = ajtalk.Smalltalk;

Smalltalk.Object.subclass_instanceVariableNames_classVariableNames_('HtmlCanvas', 'response', '');
Smalltalk.HtmlCanvas.defineMethod('write:', function(text)
{
    var self = this;
    console.log('write_');
    (function() {
        var __target = self.$response;
        return __target['write'].apply(__target, [text]);
    })()
    ;
    return self;
});
Smalltalk.HtmlCanvas.defineMethod('h1:', function(content)
{
    var self = this;
    console.log('h1_');
    self.write_('<h1>');
    self.write_(content);
    self.write_('</h1>');
    return self;
});
Smalltalk.HtmlCanvas.defineMethod('setResponse:', function(aResponse)
{
    var self = this;
    console.log('setResponse_');
    self.$response = aResponse;
    return self;
});
Smalltalk.HtmlCanvas.defineClassMethod('new:', function(aResponse)
{
    var self = this;
    console.log('new_');
    return self.new().setResponse_(aResponse);
});
