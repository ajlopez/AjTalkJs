
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
