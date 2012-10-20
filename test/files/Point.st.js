var ajtalk;
var Smalltalk;

if (typeof(ajtalk) === 'undefined')
    ajtalk = require(__dirname + '/../../lib/ajtalk.js');

if (typeof(Smalltalk) === 'undefined')
    Smalltalk = ajtalk.Smalltalk;

Smalltalk.Object.subclass_instanceVariableNames_classVariableNames_('Point', 'x y', '');
Smalltalk.Point.defineMethod('directionToLineFrom:to:', function(p1, p2)
{
    var self = this;
    console.log('directionToLineFrom_to_');
    return p2.x()['-'](p1.x())['*'](self.y()['-'](p1.y()))['-'](self.x()['-'](p1.x())['*'](p2.y()['-'](p1.y())));
});
Smalltalk.Point.defineMethod('angle', function()
{
    var self = this;
    console.log('angle');
    return self.y().arcTan_(self.x());
});
Smalltalk.Point.defineMethod('angleWith:', function(aPoint)
{
    var self = this;
    console.log('angleWith_');
    var ar = null
    var ap = null
    ar = self.angle();
    ap = aPoint.angle();
    return ap['>='](ar).ifTrue_ifFalse_(function() {
        ap['-'](ar);
        return self;
    }
    , function() {
        Float.pi()['*'](2)['-'](ar)['+'](ap);
        return self;
    }
    );
});
Smalltalk.Point.defineMethod('max', function()
{
    var self = this;
    console.log('max');
    return self.x().max_(self.y());
});
Smalltalk.Point.defineMethod('min', function()
{
    var self = this;
    console.log('min');
    return self.x().min_(self.y());
});
Smalltalk.Point.defineMethod('reflectedAbout:', function(aPoint)
{
    var self = this;
    console.log('reflectedAbout_');
    return self['-'](aPoint).negated()['+'](aPoint);
});
Smalltalk.Point.defineMethod('x', function()
{
    var self = this;
    console.log('x');
    return self.$x;
});
Smalltalk.Point.defineMethod('y', function()
{
    var self = this;
    console.log('y');
    return self.$y;
});
Smalltalk.Point.defineMethod('*', function(arg)
{
    var self = this;
    var __context = {};
    console.log('*');
    arg.isPoint().ifTrue_(function() {
        __context.value = self.$x['*'](arg.x())['@'](self.$y['*'](arg.y()));
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return arg.adaptToPoint_andSend_(self, '*');
});
Smalltalk.Point.defineMethod('+', function(arg)
{
    var self = this;
    var __context = {};
    console.log('+');
    arg.isPoint().ifTrue_(function() {
        __context.value = self.$x['+'](arg.x())['@'](self.$y['+'](arg.y()));
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return arg.adaptToPoint_andSend_(self, '+');
});
Smalltalk.Point.defineMethod('-', function(arg)
{
    var self = this;
    var __context = {};
    console.log('-');
    arg.isPoint().ifTrue_(function() {
        __context.value = self.$x['-'](arg.x())['@'](self.$y['-'](arg.y()));
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return arg.adaptToPoint_andSend_(self, '-');
});
Smalltalk.Point.defineMethod('/', function(arg)
{
    var self = this;
    var __context = {};
    console.log('/');
    arg.isPoint().ifTrue_(function() {
        __context.value = self.$x['/'](arg.x())['@'](self.$y['/'](arg.y()));
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return arg.adaptToPoint_andSend_(self, '/');
});
Smalltalk.Point.defineMethod('//', function(arg)
{
    var self = this;
    var __context = {};
    console.log('//');
    arg.isPoint().ifTrue_(function() {
        __context.value = self.$x['//'](arg.x())['@'](self.$y['//'](arg.y()));
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return arg.adaptToPoint_andSend_(self, '//');
});
Smalltalk.Point.defineMethod('\\', function(arg)
{
    var self = this;
    var __context = {};
    console.log('\\');
    arg.isPoint().ifTrue_(function() {
        __context.value = self.$x['\\'](arg.x())['@'](self.$y['\\'](arg.y()));
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return arg.adaptToPoint_andSend_(self, '\\');
});
Smalltalk.Point.defineMethod('abs', function()
{
    var self = this;
    console.log('abs');
    return self.$x.abs()['@'](self.$y.abs());
});
Smalltalk.Point.defineMethod('reciprocal', function()
{
    var self = this;
    console.log('reciprocal');
    return self.$x.reciprocal()['@'](self.$y.reciprocal());
});
Smalltalk.Point.defineMethod('<', function(aPoint)
{
    var self = this;
    console.log('<');
    return self.$x['<'](aPoint.x()).and_(function() {
        self.$y['<'](aPoint.y());
        return self;
    }
    );
});
Smalltalk.Point.defineMethod('<=', function(aPoint)
{
    var self = this;
    console.log('<=');
    return self.$x['<='](aPoint.x()).and_(function() {
        self.$y['<='](aPoint.y());
        return self;
    }
    );
});
Smalltalk.Point.defineMethod('=', function(aPoint)
{
    var self = this;
    var __context = {};
    console.log('=');
    self.species()['='](aPoint.species()).ifTrue_ifFalse_(function() {
        __context.value = self.$x['='](aPoint.x()).and_(function() {
            self.$y['='](aPoint.y());
            if (__context.return) return __context.value;
            return self;
        }
        );
        __context.return = true;
        return __context.value;
    }
    , function() {
        __context.value = false;
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return self;
});
Smalltalk.Point.defineMethod('>', function(aPoint)
{
    var self = this;
    console.log('>');
    return self.$x['>'](aPoint.x()).and_(function() {
        self.$y['>'](aPoint.y());
        return self;
    }
    );
});
Smalltalk.Point.defineMethod('>=', function(aPoint)
{
    var self = this;
    console.log('>=');
    return self.$x['>='](aPoint.x()).and_(function() {
        self.$y['>='](aPoint.y());
        return self;
    }
    );
});
Smalltalk.Point.defineMethod('closeTo:', function(aPoint)
{
    var self = this;
    console.log('closeTo_');
    return self.$x.closeTo_(aPoint.x()).and_(function() {
        self.$y.closeTo_(aPoint.y());
        return self;
    }
    );
});
Smalltalk.Point.defineMethod('hash', function()
{
    var self = this;
    console.log('hash');
    return self.$x.hash().hashMultiply()['+'](self.$y.hash()).hashMultiply();
});
Smalltalk.Point.defineMethod('max:', function(aPoint)
{
    var self = this;
    console.log('max_');
    return self.$x.max_(aPoint.x())['@'](self.$y.max_(aPoint.y()));
});
Smalltalk.Point.defineMethod('min:', function(aPoint)
{
    var self = this;
    console.log('min_');
    return self.$x.min_(aPoint.x())['@'](self.$y.min_(aPoint.y()));
});
Smalltalk.Point.defineMethod('min:max:', function(aMin, aMax)
{
    var self = this;
    console.log('min_max_');
    return self.min_(aMin).max_(aMax);
});
Smalltalk.Point.defineMethod('adaptToCollection:andSend:', function(rcvr, selector)
{
    var self = this;
    console.log('adaptToCollection_andSend_');
    return rcvr.collect_(function(element) {
        element.perform_with_(selector, self);
        return self;
    }
    );
});
Smalltalk.Point.defineMethod('adaptToNumber:andSend:', function(rcvr, selector)
{
    var self = this;
    console.log('adaptToNumber_andSend_');
    return rcvr['@'](rcvr).perform_with_(selector, self);
});
Smalltalk.Point.defineMethod('adaptToString:andSend:', function(rcvr, selector)
{
    var self = this;
    console.log('adaptToString_andSend_');
    return rcvr.asNumber().perform_with_(selector, self);
});
Smalltalk.Point.defineMethod('asFloatPoint', function()
{
    var self = this;
    console.log('asFloatPoint');
    return self.$x.asFloat()['@'](self.$y.asFloat());
});
Smalltalk.Point.defineMethod('asIntegerPoint', function()
{
    var self = this;
    console.log('asIntegerPoint');
    return self.$x.asInteger()['@'](self.$y.asInteger());
});
Smalltalk.Point.defineMethod('asNonFractionalPoint', function()
{
    var self = this;
    var __context = {};
    console.log('asNonFractionalPoint');
    self.$x.isFraction().or_(function() {
        self.$y.isFraction();
        if (__context.return) return __context.value;
        return self;
    }
    ).ifTrue_(function() {
        __context.value = self.$x.asFloat()['@'](self.$y.asFloat());
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return self;
});
Smalltalk.Point.defineMethod('asPoint', function()
{
    var self = this;
    console.log('asPoint');
    return self;
});
Smalltalk.Point.defineMethod('corner:', function(aPoint)
{
    var self = this;
    console.log('corner_');
    return Rectangle.origin_corner_(self, aPoint);
});
Smalltalk.Point.defineMethod('extent:', function(aPoint)
{
    var self = this;
    console.log('extent_');
    return Rectangle.origin_extent_(self, aPoint);
});
Smalltalk.Point.defineMethod('isPoint', function()
{
    var self = this;
    console.log('isPoint');
    return true;
});
Smalltalk.Point.defineMethod('rect:', function(aPoint)
{
    var self = this;
    console.log('rect_');
    return Rectangle.origin_corner_(self.min_(aPoint), self.max_(aPoint));
});
Smalltalk.Point.defineMethod('deepCopy', function()
{
    var self = this;
    console.log('deepCopy');
    return self.$x.deepCopy()['@'](self.$y.deepCopy());
});
Smalltalk.Point.defineMethod('veryDeepCopyWith:', function(deepCopier)
{
    var self = this;
    console.log('veryDeepCopyWith_');
});
Smalltalk.Point.defineMethod('guarded', function()
{
    var self = this;
    console.log('guarded');
    self.max_(1['@'](1));
    return self;
});
Smalltalk.Point.defineMethod('scaleTo:', function(anExtent)
{
    var self = this;
    var __context = {};
    console.log('scaleTo_');
    var factor = null
    var sX = null
    var sY = null
    factor = 3['reciprocal']();
    sX = anExtent.x()['/'](self.x().asFloat());
    sY = anExtent.y()['/'](self.y().asFloat());
    sX['='](sY).ifTrue_(function() {
        __context.value = sX['@'](sY);
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return sX['<'](sY).ifTrue_ifFalse_(function() {
        sX['@'](sX.max_(sY['*'](factor)));
        if (__context.return) return __context.value;
        return self;
    }
    , function() {
        sY.max_(sX['*'](factor))['@'](sY);
        if (__context.return) return __context.value;
        return self;
    }
    );
});
Smalltalk.Point.defineMethod('isInsideCircle:with:with:', function(a, b, c)
{
    var self = this;
    console.log('isInsideCircle_with_with_');
    return a.dotProduct_(a)['*'](b.triangleArea_with_(c, self))['-'](b.dotProduct_(b)['*'](a.triangleArea_with_(c, self)))['+'](c.dotProduct_(c)['*'](a.triangleArea_with_(b, self)))['-'](self.dotProduct_(self)['*'](a.triangleArea_with_(b, c)))['>'](0);
});
Smalltalk.Point.defineMethod('sideOf:', function(otherPoint)
{
    var self = this;
    console.log('sideOf_');
    var side = null
    side = self.crossProduct_(otherPoint).sign();
    return ['right', 'center', 'left'].at_(side['+'](2));
});
Smalltalk.Point.defineMethod('to:intersects:to:', function(end1, start2, end2)
{
    var self = this;
    var __context = {};
    console.log('to_intersects_to_');
    var start1 = null
    var sideStart = null
    var sideEnd = null
    start1 = self;
    start1['='](start2).or_(function() {
        end1['='](end2);
        if (__context.return) return __context.value;
        return self;
    }
    ).or_(function() {
        start1['='](end2);
        if (__context.return) return __context.value;
        return self;
    }
    ).or_(function() {
        start2['='](end1);
        if (__context.return) return __context.value;
        return self;
    }
    ).ifTrue_(function() {
        __context.value = true;
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    sideStart = start1.to_sideOf_(end1, start2);
    sideEnd = start1.to_sideOf_(end1, end2);
    sideStart['='](sideEnd).ifTrue_(function() {
        __context.value = false;
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    sideStart = start2.to_sideOf_(end2, start1);
    sideEnd = start2.to_sideOf_(end2, end1);
    sideStart['='](sideEnd).ifTrue_(function() {
        __context.value = false;
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return true;
});
Smalltalk.Point.defineMethod('to:sideOf:', function(end, otherPoint)
{
    var self = this;
    console.log('to_sideOf_');
    return end['-'](self).sideOf_(otherPoint['-'](self));
});
Smalltalk.Point.defineMethod('triangleArea:with:', function(b, c)
{
    var self = this;
    console.log('triangleArea_with_');
    return b.x()['-'](self.x())['*'](c.y()['-'](self.y()))['-'](b.y()['-'](self.y())['*'](c.x()['-'](self.x())));
});
Smalltalk.Point.defineMethod('interpolateTo:at:', function(end, amountDone)
{
    var self = this;
    console.log('interpolateTo_at_');
    return self['+'](end['-'](self)['*'](amountDone));
});
Smalltalk.Point.defineMethod('bearingToPoint:', function(anotherPoint)
{
    var self = this;
    var __context = {};
    console.log('bearingToPoint_');
    var deltaX = null
    var deltaY = null
    deltaX = anotherPoint.x()['-'](self.$x);
    deltaY = anotherPoint.y()['-'](self.$y);
    deltaX.abs()['<'](0.001).ifTrue_(function() {
        __context.value = deltaY['>'](0).ifTrue_ifFalse_(function() {
            180;
            return self;
        }
        , function() {
            0;
            return self;
        }
        );
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return deltaX['>='](0).ifTrue_ifFalse_(function() {
        90;
        return self;
    }
    , function() {
        270;
        return self;
    }
    )['-'](deltaY['/'](deltaX).arcTan().negated().radiansToDegrees()).rounded();
});
Smalltalk.Point.defineMethod('crossProduct:', function(aPoint)
{
    var self = this;
    console.log('crossProduct_');
    return self.$x['*'](aPoint.y())['-'](self.$y['*'](aPoint.x()));
});
Smalltalk.Point.defineMethod('dist:', function(aPoint)
{
    var self = this;
    console.log('dist_');
    var dx = null
    var dy = null
    dx = aPoint.x()['-'](self.$x);
    dy = aPoint.y()['-'](self.$y);
    return dx['*'](dx)['+'](dy['*'](dy)).sqrt();
});
Smalltalk.Point.defineMethod('dotProduct:', function(aPoint)
{
    var self = this;
    console.log('dotProduct_');
    return self.$x['*'](aPoint.x())['+'](self.$y['*'](aPoint.y()));
});
Smalltalk.Point.defineMethod('eightNeighbors', function()
{
    var self = this;
    console.log('eightNeighbors');
    return [self['+'](1['@'](0)), self['+'](1['@'](1)), self['+'](0['@'](1)), self['+'](1['-']()['@'](1)), self['+'](1['-']()['@'](0)), self['+'](1['-']()['@'](1['-']())), self['+'](0['@'](1['-']())), self['+'](1['@'](1['-']()))];
});
Smalltalk.Point.defineMethod('flipBy:centerAt:', function(direction, c)
{
    var self = this;
    var __context = {};
    console.log('flipBy_centerAt_');
    direction['==']('vertical').ifTrue_(function() {
        __context.value = self.$x['@'](c.y()['*'](2)['-'](self.$y));
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    direction['==']('horizontal').ifTrue_(function() {
        __context.value = c.x()['*'](2)['-'](self.$x)['@'](self.$y);
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    self.error_('unrecognizable direction');
    if (__context.return) return __context.value;
    return self;
});
Smalltalk.Point.defineMethod('fourDirections', function()
{
    var self = this;
    console.log('fourDirections');
    return Array.with_with_with_with_(self.leftRotated(), self.negated(), self.rightRotated(), self);
});
Smalltalk.Point.defineMethod('fourNeighbors', function()
{
    var self = this;
    console.log('fourNeighbors');
    return Array.with_with_with_with_(self['+'](1['@'](0)), self['+'](0['@'](1)), self['+'](1['-']()['@'](0)), self['+'](0['@'](1['-']())));
});
Smalltalk.Point.defineMethod('grid:', function(aPoint)
{
    var self = this;
    console.log('grid_');
    var newX = null
    var newY = null
    newX = self.$x['+'](aPoint.x()['//'](2)).truncateTo_(aPoint.x());
    newY = self.$y['+'](aPoint.y()['//'](2)).truncateTo_(aPoint.y());
    return newX['@'](newY);
});
Smalltalk.Point.defineMethod('insideTriangle:with:with:', function(p1, p2, p3)
{
    var self = this;
    var __context = {};
    console.log('insideTriangle_with_with_');
    var p0 = null
    var b0 = null
    var b1 = null
    var b2 = null
    var b3 = null
    p0 = self;
    b0 = p2.x()['-'](p1.x())['*'](p3.y()['-'](p1.y()))['-'](p3.x()['-'](p1.x())['*'](p2.y()['-'](p1.y())));
    b0.isZero().ifTrue_(function() {
        __context.value = false;
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    b0 = 1['/'](b0);
    b1 = p2.x()['-'](p0.x())['*'](p3.y()['-'](p0.y()))['-'](p3.x()['-'](p0.x())['*'](p2.y()['-'](p0.y())))['*'](b0);
    b2 = p3.x()['-'](p0.x())['*'](p1.y()['-'](p0.y()))['-'](p1.x()['-'](p0.x())['*'](p3.y()['-'](p0.y())))['*'](b0);
    b3 = p1.x()['-'](p0.x())['*'](p2.y()['-'](p0.y()))['-'](p2.x()['-'](p0.x())['*'](p1.y()['-'](p0.y())))['*'](b0);
    b1['<'](0).ifTrue_(function() {
        __context.value = false;
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    b2['<'](0).ifTrue_(function() {
        __context.value = false;
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    b3['<'](0).ifTrue_(function() {
        __context.value = false;
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return true;
});
Smalltalk.Point.defineMethod('leftRotated', function()
{
    var self = this;
    console.log('leftRotated');
    return self.$y['@'](self.$x.negated());
});
Smalltalk.Point.defineMethod('nearestPointAlongLineFrom:to:', function(p1, p2)
{
    var self = this;
    var __context = {};
    console.log('nearestPointAlongLineFrom_to_');
    var x21 = null
    var y21 = null
    var t = null
    var x1 = null
    var y1 = null
    p1.x()['='](p2.x()).ifTrue_(function() {
        __context.value = p1.x()['@'](self.$y);
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    p1.y()['='](p2.y()).ifTrue_(function() {
        __context.value = self.$x['@'](p1.y());
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    x1 = p1.x().asFloat();
    y1 = p1.y().asFloat();
    x21 = p2.x().asFloat()['-'](x1);
    y21 = p2.y().asFloat()['-'](y1);
    t = self.$y.asFloat()['-'](y1)['/'](x21)['+'](self.$x.asFloat()['-'](x1)['/'](y21))['/'](x21['/'](y21)['+'](y21['/'](x21)));
    return x1['+'](t['*'](x21))['@'](y1['+'](t['*'](y21)));
});
Smalltalk.Point.defineMethod('nearestPointOnLineFrom:to:', function(p1, p2)
{
    var self = this;
    console.log('nearestPointOnLineFrom_to_');
    return self.nearestPointAlongLineFrom_to_(p1, p2).adhereTo_(p1.rect_(p2));
});
Smalltalk.Point.defineMethod('normal', function()
{
    var self = this;
    var __context = {};
    console.log('normal');
    var n = null
    var d = null
    n = self.$y.negated()['@'](self.$x);
    d = n.x()['*'](n.x())['+'](n.y()['*'](n.y()))['='](0).ifTrue_(function() {
        __context.value = 1['-']()['@'](0);
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return n['/'](d.sqrt());
});
Smalltalk.Point.defineMethod('normalized', function()
{
    var self = this;
    console.log('normalized');
    var r = null
    r = self.$x['*'](self.$x)['+'](self.$y['*'](self.$y)).sqrt();
    return self.$x['/'](r)['@'](self.$y['/'](r));
});
Smalltalk.Point.defineMethod('octantOf:', function(otherPoint)
{
    var self = this;
    var __context = {};
    console.log('octantOf_');
    var quad = null
    var moreHoriz = null
    self.$x['='](otherPoint.x()).and_(function() {
        self.$y['>'](otherPoint.y());
        if (__context.return) return __context.value;
        return self;
    }
    ).ifTrue_(function() {
        __context.value = 6;
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    self.$y['='](otherPoint.y()).and_(function() {
        self.$x['<'](otherPoint.x());
        if (__context.return) return __context.value;
        return self;
    }
    ).ifTrue_(function() {
        __context.value = 8;
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    quad = self.quadrantOf_(otherPoint);
    moreHoriz = self.$x['-'](otherPoint.x()).abs()['>='](self.$y['-'](otherPoint.y()).abs());
    quad.even().eqv_(moreHoriz).ifTrue_ifFalse_(function() {
        __context.value = quad['*'](2);
        __context.return = true;
        return __context.value;
    }
    , function() {
        __context.value = quad['*'](2)['-'](1);
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return self;
});
Smalltalk.Point.defineMethod('onLineFrom:to:', function(p1, p2)
{
    var self = this;
    console.log('onLineFrom_to_');
    return self.onLineFrom_to_within_(p1, p2, 2);
});
Smalltalk.Point.defineMethod('onLineFrom:to:within:', function(p1, p2, epsilon)
{
    var self = this;
    var __context = {};
    console.log('onLineFrom_to_within_');
    p1.x()['<'](p2.x()).ifTrue_ifFalse_(function() {
        self.$x['<'](p1.x()['-'](epsilon)).or_(function() {
            self.$x['>'](p2.x()['+'](epsilon));
            if (__context.return) return __context.value;
            return self;
        }
        ).ifTrue_(function() {
            __context.value = false;
            __context.return = true;
            return __context.value;
        }
        );
        if (__context.return) return __context.value;
        return self;
    }
    , function() {
        self.$x['<'](p2.x()['-'](epsilon)).or_(function() {
            self.$x['>'](p1.x()['+'](epsilon));
            if (__context.return) return __context.value;
            return self;
        }
        ).ifTrue_(function() {
            __context.value = false;
            __context.return = true;
            return __context.value;
        }
        );
        if (__context.return) return __context.value;
        return self;
    }
    );
    if (__context.return) return __context.value;
    p1.y()['<'](p2.y()).ifTrue_ifFalse_(function() {
        self.$y['<'](p1.y()['-'](epsilon)).or_(function() {
            self.$y['>'](p2.y()['+'](epsilon));
            if (__context.return) return __context.value;
            return self;
        }
        ).ifTrue_(function() {
            __context.value = false;
            __context.return = true;
            return __context.value;
        }
        );
        if (__context.return) return __context.value;
        return self;
    }
    , function() {
        self.$y['<'](p2.y()['-'](epsilon)).or_(function() {
            self.$y['>'](p1.y()['+'](epsilon));
            if (__context.return) return __context.value;
            return self;
        }
        ).ifTrue_(function() {
            __context.value = false;
            __context.return = true;
            return __context.value;
        }
        );
        if (__context.return) return __context.value;
        return self;
    }
    );
    if (__context.return) return __context.value;
    return self.dist_(self.nearestPointAlongLineFrom_to_(p1, p2))['<='](epsilon);
});
Smalltalk.Point.defineMethod('quadrantOf:', function(otherPoint)
{
    var self = this;
    console.log('quadrantOf_');
    return self.$x['<='](otherPoint.x()).ifTrue_ifFalse_(function() {
        self.$y['<'](otherPoint.y()).ifTrue_ifFalse_(function() {
            1;
            return self;
        }
        , function() {
            4;
            return self;
        }
        );
        return self;
    }
    , function() {
        self.$y['<='](otherPoint.y()).ifTrue_ifFalse_(function() {
            2;
            return self;
        }
        , function() {
            3;
            return self;
        }
        );
        return self;
    }
    );
});
Smalltalk.Point.defineMethod('rightRotated', function()
{
    var self = this;
    console.log('rightRotated');
    return self.$y.negated()['@'](self.$x);
});
Smalltalk.Point.defineMethod('rotateBy:centerAt:', function(direction, c)
{
    var self = this;
    var __context = {};
    console.log('rotateBy_centerAt_');
    var offset = null
    offset = self['-'](c);
    direction['==']('right').ifTrue_(function() {
        __context.value = offset.y().negated()['@'](offset.x())['+'](c);
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    direction['==']('left').ifTrue_(function() {
        __context.value = offset.y()['@'](offset.x().negated())['+'](c);
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    direction['==']('pi').ifTrue_(function() {
        __context.value = c['-'](offset);
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    self.error_('unrecognizable direction');
    if (__context.return) return __context.value;
    return self;
});
Smalltalk.Point.defineMethod('sign', function()
{
    var self = this;
    console.log('sign');
    return self.$x.sign()['@'](self.$y.sign());
});
Smalltalk.Point.defineMethod('sortsBefore:', function(otherPoint)
{
    var self = this;
    console.log('sortsBefore_');
    return self.$y['='](otherPoint.y()).ifTrue_ifFalse_(function() {
        self.$x['<='](otherPoint.x());
        return self;
    }
    , function() {
        self.$y['<='](otherPoint.y());
        return self;
    }
    );
});
Smalltalk.Point.defineMethod('squaredDistanceTo:', function(aPoint)
{
    var self = this;
    console.log('squaredDistanceTo_');
    var delta = null
    delta = aPoint['-'](self);
    return delta.dotProduct_(delta);
});
Smalltalk.Point.defineMethod('transposed', function()
{
    var self = this;
    console.log('transposed');
    return self.$y['@'](self.$x);
});
Smalltalk.Point.defineMethod('degrees', function()
{
    var self = this;
    var __context = {};
    console.log('degrees');
    var tan = null
    var theta = null
    self.$x['='](0).ifTrue_ifFalse_(function() {
        self.$y['>='](0).ifTrue_ifFalse_(function() {
            __context.value = 90;
            __context.return = true;
            return __context.value;
        }
        , function() {
            __context.value = 270;
            __context.return = true;
            return __context.value;
        }
        );
        if (__context.return) return __context.value;
        return self;
    }
    , function() {
        tan = self.$y.asFloat()['/'](self.$x.asFloat());
        theta = tan.arcTan();
        self.$x['>='](0).ifTrue_ifFalse_(function() {
            self.$y['>='](0).ifTrue_ifFalse_(function() {
                __context.value = theta.radiansToDegrees();
                __context.return = true;
                return __context.value;
            }
            , function() {
                __context.value = 360['+'](theta.radiansToDegrees());
                __context.return = true;
                return __context.value;
            }
            );
            if (__context.return) return __context.value;
            return self;
        }
        , function() {
            __context.value = 180['+'](theta.radiansToDegrees());
            __context.return = true;
            return __context.value;
        }
        );
        if (__context.return) return __context.value;
        return self;
    }
    );
    if (__context.return) return __context.value;
    return self;
});
Smalltalk.Point.defineMethod('r', function()
{
    var self = this;
    console.log('r');
    return self.dotProduct_(self).sqrt();
});
Smalltalk.Point.defineMethod('theta', function()
{
    var self = this;
    var __context = {};
    console.log('theta');
    var tan = null
    var theta = null
    self.$x['='](0).ifTrue_ifFalse_(function() {
        self.$y['>='](0).ifTrue_ifFalse_(function() {
            __context.value = 1.5707963267949;
            __context.return = true;
            return __context.value;
        }
        , function() {
            __context.value = 4.71238898038469;
            __context.return = true;
            return __context.value;
        }
        );
        if (__context.return) return __context.value;
        return self;
    }
    , function() {
        tan = self.$y.asFloat()['/'](self.$x.asFloat());
        theta = tan.arcTan();
        self.$x['>='](0).ifTrue_ifFalse_(function() {
            self.$y['>='](0).ifTrue_ifFalse_(function() {
                __context.value = theta;
                __context.return = true;
                return __context.value;
            }
            , function() {
                __context.value = 6.28318530717959['+'](theta);
                __context.return = true;
                return __context.value;
            }
            );
            if (__context.return) return __context.value;
            return self;
        }
        , function() {
            __context.value = 3.14159265358979['+'](theta);
            __context.return = true;
            return __context.value;
        }
        );
        if (__context.return) return __context.value;
        return self;
    }
    );
    if (__context.return) return __context.value;
    return self;
});
Smalltalk.Point.defineMethod('printOn:', function(aStream)
{
    var self = this;
    var __context = {};
    console.log('printOn_');
    self.$x.printOn_(aStream);
    if (__context.return) return __context.value;
    aStream.nextPut_('@');
    if (__context.return) return __context.value;
    self.$y.notNil().and_(function() {
        self.$y.negative();
        if (__context.return) return __context.value;
        return self;
    }
    ).ifTrue_(function() {
        aStream.space();
        if (__context.return) return __context.value;
        return self;
    }
    );
    if (__context.return) return __context.value;
    self.$y.printOn_(aStream);
    if (__context.return) return __context.value;
    return self;
});
Smalltalk.Point.defineMethod('storeOn:', function(aStream)
{
    var self = this;
    console.log('storeOn_');
    aStream.nextPut_('(');
    self.printOn_(aStream);
    aStream.nextPut_(')');
    return self;
});
Smalltalk.Point.defineMethod('isSelfEvaluating', function()
{
    var self = this;
    console.log('isSelfEvaluating');
    return self.class()['=='](Point);
});
Smalltalk.Point.defineMethod('isZero', function()
{
    var self = this;
    console.log('isZero');
    return self.$x.isZero().and_(function() {
        self.$y.isZero();
        return self;
    }
    );
});
Smalltalk.Point.defineMethod('adhereTo:', function(aRectangle)
{
    var self = this;
    var __context = {};
    console.log('adhereTo_');
    aRectangle.containsPoint_(self).ifTrue_(function() {
        __context.value = self;
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return self.$x.max_(aRectangle.left()).min_(aRectangle.right())['@'](self.$y.max_(aRectangle.top()).min_(aRectangle.bottom()));
});
Smalltalk.Point.defineMethod('negated', function()
{
    var self = this;
    console.log('negated');
    return 0['-'](self.$x)['@'](0['-'](self.$y));
});
Smalltalk.Point.defineMethod('rotateBy:about:', function(angle, center)
{
    var self = this;
    console.log('rotateBy_about_');
    var p = null
    var r = null
    var theta = null
    p = self['-'](center);
    r = p.r();
    theta = angle.asFloat()['-'](p.theta());
    return center.x().asFloat()['+'](r['*'](theta.cos()))['@'](center.y().asFloat()['-'](r['*'](theta.sin())));
});
Smalltalk.Point.defineMethod('scaleBy:', function(factor)
{
    var self = this;
    console.log('scaleBy_');
    return factor.x()['*'](self.$x)['@'](factor.y()['*'](self.$y));
});
Smalltalk.Point.defineMethod('scaleFrom:to:', function(rect1, rect2)
{
    var self = this;
    console.log('scaleFrom_to_');
    return rect2.topLeft()['+'](self.$x['-'](rect1.left())['*'](rect2.width())['//'](rect1.width())['@'](self.$y['-'](rect1.top())['*'](rect2.height())['//'](rect1.height())));
});
Smalltalk.Point.defineMethod('translateBy:', function(delta)
{
    var self = this;
    console.log('translateBy_');
    return delta.x()['+'](self.$x)['@'](delta.y()['+'](self.$y));
});
Smalltalk.Point.defineMethod('rounded', function()
{
    var self = this;
    var __context = {};
    console.log('rounded');
    self.$x.isInteger().and_(function() {
        self.$y.isInteger();
        if (__context.return) return __context.value;
        return self;
    }
    ).ifTrue_(function() {
        __context.value = self;
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return self.$x.rounded()['@'](self.$y.rounded());
});
Smalltalk.Point.defineMethod('roundTo:', function(grid)
{
    var self = this;
    console.log('roundTo_');
    var gridPoint = null
    gridPoint = grid.asPoint();
    return self.$x.roundTo_(gridPoint.x())['@'](self.$y.roundTo_(gridPoint.y()));
});
Smalltalk.Point.defineMethod('truncateTo:', function(grid)
{
    var self = this;
    console.log('truncateTo_');
    var gridPoint = null
    gridPoint = grid.asPoint();
    return self.$x.truncateTo_(gridPoint.x())['@'](self.$y.truncateTo_(gridPoint.y()));
});
Smalltalk.Point.defineMethod('truncated', function()
{
    var self = this;
    var __context = {};
    console.log('truncated');
    self.$x.isInteger().and_(function() {
        self.$y.isInteger();
        if (__context.return) return __context.value;
        return self;
    }
    ).ifTrue_(function() {
        __context.value = self;
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return self.$x.truncated()['@'](self.$y.truncated());
});
Smalltalk.Point.defineMethod('ceiling', function()
{
    var self = this;
    var __context = {};
    console.log('ceiling');
    self.$x.isInteger().and_(function() {
        self.$y.isInteger();
        if (__context.return) return __context.value;
        return self;
    }
    ).ifTrue_(function() {
        __context.value = self;
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return self.$x.ceiling()['@'](self.$y.ceiling());
});
Smalltalk.Point.defineMethod('floor', function()
{
    var self = this;
    var __context = {};
    console.log('floor');
    self.$x.isInteger().and_(function() {
        self.$y.isInteger();
        if (__context.return) return __context.value;
        return self;
    }
    ).ifTrue_(function() {
        __context.value = self;
        __context.return = true;
        return __context.value;
    }
    );
    if (__context.return) return __context.value;
    return self.$x.floor()['@'](self.$y.floor());
});
Smalltalk.Point.defineMethod('isIntegerPoint', function()
{
    var self = this;
    console.log('isIntegerPoint');
    return self.$x.isInteger().and_(function() {
        self.$y.isInteger();
        return self;
    }
    );
});
Smalltalk.Point.defineMethod('roundDownTo:', function(grid)
{
    var self = this;
    console.log('roundDownTo_');
    var gridPoint = null
    gridPoint = grid.asPoint();
    return self.$x.roundDownTo_(gridPoint.x())['@'](self.$y.roundDownTo_(gridPoint.y()));
});
Smalltalk.Point.defineMethod('roundUpTo:', function(grid)
{
    var self = this;
    console.log('roundUpTo_');
    var gridPoint = null
    gridPoint = grid.asPoint();
    return self.$x.roundUpTo_(gridPoint.x())['@'](self.$y.roundUpTo_(gridPoint.y()));
});
Smalltalk.Point.defineMethod('bitShiftPoint:', function(bits)
{
    var self = this;
    console.log('bitShiftPoint_');
    self.$x = self.$x.bitShift_(bits);
    self.$y = self.$y.bitShift_(bits);
    return self;
});
Smalltalk.Point.defineMethod('setR:degrees:', function(rho, degrees)
{
    var self = this;
    console.log('setR_degrees_');
    var radians = null
    radians = degrees.asFloat().degreesToRadians();
    self.$x = rho.asFloat()['*'](radians.cos());
    self.$y = rho.asFloat()['*'](radians.sin());
    return self;
});
Smalltalk.Point.defineMethod('setX:setY:', function(xValue, yValue)
{
    var self = this;
    console.log('setX_setY_');
    self.$x = xValue;
    self.$y = yValue;
    return self;
});
Smalltalk.Point.defineClassMethod('settingInputWidgetForNode:', function(aSettingNode)
{
    var self = this;
    console.log('settingInputWidgetForNode_');
    return aSettingNode.inputWidgetForPoint();
});
Smalltalk.Point.defineClassMethod('fromUser', function()
{
    var self = this;
    console.log('fromUser');
    Sensor.waitNoButton();
    Cursor.crossHair().show();
    Sensor.waitButton();
    Cursor.normal().show();
    return Sensor.cursorPoint();
});
Smalltalk.Point.defineClassMethod('fromUserWithCursor:', function(aCursor)
{
    var self = this;
    var __context = {};
    console.log('fromUserWithCursor_');
    Sensor.waitNoButton();
    if (__context.return) return __context.value;
    aCursor.showWhile_(function() {
        Sensor.waitButton();
        if (__context.return) return __context.value;
        return self;
    }
    );
    if (__context.return) return __context.value;
    return Sensor.cursorPoint();
});
Smalltalk.Point.defineClassMethod('r:degrees:', function(rho, degrees)
{
    var self = this;
    console.log('r_degrees_');
    return self.basicNew().setR_degrees_(rho, degrees);
});
Smalltalk.Point.defineClassMethod('x:y:', function(xInteger, yInteger)
{
    var self = this;
    console.log('x_y_');
    return self.basicNew().setX_setY_(xInteger, yInteger);
});
