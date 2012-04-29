
var http = require('http');
var ajtalk = require('../../../lib/ajtalk');
ajtalk.load('HtmlCanvas.st');
ajtalk.load('HtmlHelloPage.st');
var Smalltalk = ajtalk.Smalltalk;

console.log('AjTalk loaded');

var page = Smalltalk.HtmlHelloPage.basicNew();

function Response(res) {
	this._write_ = function(text) { res.write(text); }
}

http.createServer(function(req,res) {
	var html = Smalltalk.HtmlCanvas.new_(res);
	page.render_(html);
	res.end();
}).listen(8080);

console.log('Server started at http://127.0.0.1:8080');
