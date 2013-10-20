
var http = require('http');
var ajtalk = require('../..');
ajtalk.load('HtmlCanvas.st');
ajtalk.load('HtmlHelloPage.st');
var Smalltalk = ajtalk.Smalltalk;

console.log('AjTalk loaded');

var page = Smalltalk.HtmlHelloPage.basicNew();

http.createServer(function(req,res) {
	var html = Smalltalk.HtmlCanvas.new_(res);
	page.render_(html);
	res.end();
}).listen(3000);

console.log('Server started at http://localhost:3000');
