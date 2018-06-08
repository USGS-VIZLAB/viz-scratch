var left = function(){
	$('#output').html('<p>The function left() fired from buttons.js</p>');
};

var right = function(){
	$('#output').html('<p>The function right() fired from buttons.js</p>');
};

module.exports = {
	left: left,
	right: right
};