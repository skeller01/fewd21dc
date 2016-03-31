

$(document).ready(function(){

//define variables
var lights="on";

//define function for lights on and off

function   switchLights(){
	if(lights=="on"){
		lights="off";
		$("body").addClass("dark");
	}else{
		lights="on";
		$("body").removeClass("dark");
	}
}

//attach switch lights to the light switch on the screen 
$("#light_switch").click(switchLights);






});