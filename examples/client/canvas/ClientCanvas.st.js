var ajtalk;
var Smalltalk;

if (typeof(ajtalk) === 'undefined')
    ajtalk = require('./lib/ajtalk.js');

if (typeof(Smalltalk) === 'undefined')
    Smalltalk = ajtalk.Smalltalk;

Smalltalk.Object.subclass_instanceVariableNames_classVariableNames_('ClientCanvas', 'html5canvas', '');
Smalltalk.ClientCanvas.defineMethod('drawLine:to:', function(fromPoint, toPoint)
{
    var self = this;
    console.log('drawLine_to_');
    self.$html5canvas.x1_y1_x2_y2_(fromPoint.x(), fromPoint.y(), toPoint.x(), toPoint.y());
});
Smalltalk.ClientCanvas.defineMethod('example1', function()
{
    var self = this;
    console.log('example1');
    self.drawLine_to_(10['@'](10), 100['@'](100));
});
Smalltalk.ClientCanvas.defineMethod('example2', function()
{
    var self = this;
    var __context = {};
    console.log('example2');
    10['do_'](function(k) {
        self.drawLine_to_(k['*'](10)['@'](k['*'](5)), k['*'](20)['@'](k['*'](25)));
        if (__context.return) return __context.value;
    }
    );
    if (__context.return) return __context.value;
});
