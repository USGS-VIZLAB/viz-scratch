var inputFunc = function(){
	var inputVal = $('#newModule input').val();
	$('#output2').html('<p>' + inputVal + '</p>');
	$('#newModule input').val('');
};



module.exports = {
	inputFunc: inputFunc
};