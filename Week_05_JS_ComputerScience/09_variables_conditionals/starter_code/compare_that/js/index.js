$(document).ready(function(){

	//initialize our comparison value
	var comparison;

	
	//build comparison function
	function compare(){
		var a  = $("#a").val();
		var b = $("#b").val();

		if(a<b){
			comparison = "<";
		}
		else if(a>b){
			comparison=">";
		}
		else if(a === b){
			comparison="=";
		}
		else{
			comparison="N/A";
		}
		$("#comparison").html(comparison);
	}

	//click compare
	$("#submit").click(compare);


});