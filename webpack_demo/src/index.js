import './less/main.less';
var button = require('./buttons/js/buttons.js');

$('#buttonOne').on('click', function(){
	button.left();
});

$('#buttonTwo').on('click', function(){
	button.right();
});