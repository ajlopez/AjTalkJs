
module.exports = {
	evaluate: function(text)
	{
		return eval(text);
	},
	execute: function(text)
	{
		return eval("(function(){" + text + "})()");
	}
};