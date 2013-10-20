
var chunkreader = require('../lib/chunkreader'),
    path = require('path');
    fs = require('fs');
    
var filename = path.join(__dirname, '..', 'pharo', 'Point.st');
var text = fs.readFileSync(filename).toString();

exports['get first chunk'] = function (test) {
    var reader = chunkreader.createReader(text);
    var chunk = reader.nextChunk();
    
    test.ok(chunk);
    test.equal(chunk, "'From Pharo1.3 of 16 June 2011 [Latest update: #13315] on 29 October 2011 at 3:52:13 pm'");
};

exports['get second chunk'] = function (test) {
    var reader = chunkreader.createReader(text);
    reader.nextChunk();
    var chunk = reader.nextChunk();
    
    test.ok(chunk);
    test.equal(chunk, "Object subclass: #Point\r\
\tinstanceVariableNames: 'x y'\r\
\tclassVariableNames: ''\r\
\tpoolDictionaries: ''\r\
\tcategory: 'Graphics-Primitives'");
};

exports['get third chunk'] = function (test) {
    var reader = chunkreader.createReader(text);
    reader.nextChunk();
    reader.nextChunk();
    var chunk = reader.nextChunk();
    
    test.ok(chunk);
    test.equal(chunk, "!Point commentStamp: '<historical>' prior: 0");
};

exports['get fourth chunk'] = function (test) {
    var reader = chunkreader.createReader(text);
    reader.nextChunk();
    reader.nextChunk();
    reader.nextChunk();
    var chunk = reader.nextChunk();
    
    test.ok(chunk);
    test.equal(chunk, "I represent an x-y pair of numbers usually designating a location on the screen.");
};

exports['get fifth chunk'] = function (test) {
    var reader = chunkreader.createReader(text);
    reader.nextChunk();
    reader.nextChunk();
    reader.nextChunk();
    reader.nextChunk();
    var chunk = reader.nextChunk();
    
    test.ok(chunk);
    test.equal(chunk, "!Point methodsFor: '*Polymorph-Geometry' stamp: 'gvc 10/31/2006 11:01'");
};

exports['get seventh empty'] = function (test) {
    var reader = chunkreader.createReader(text);
    reader.nextChunk();
    reader.nextChunk();
    reader.nextChunk();
    reader.nextChunk();
    reader.nextChunk();
    reader.nextChunk();
    var chunk = reader.nextChunk();
    
    test.equal(chunk, '');
};
