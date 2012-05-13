var ajtalk;
var Smalltalk;

if (typeof(ajtalk) === 'undefined')
    ajtalk = require('ajtalk.js');

if (typeof(Smalltalk) === 'undefined')
    Smalltalk = ajtalk.Smalltalk;

Smalltalk.Object.subclass_instanceVariableNames_classVariableNames_('HtmlHelloPage', '', '');
Smalltalk.HtmlHelloPage.defineMethod('render:', function(html)
{
    var self = this;
    console.log('render_');
    html.h1_('Hello, world');
    return self;
});
