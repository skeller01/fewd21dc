$(document).ready(function(){

//declare global variable
var total=0;

//create the add10 function
function add10(){
	total=total+10;
	$("#result").html(total);

}

function add5(){
	total=total+5;
	$("#result").html(total);

}

function sub1(){
	total=total-1;
	$("#result").html(total);

}

function zero(){
	total=0;
	$("#result").html(total);

}

//add ten 
$("#add10").click(add10);
$("#add5").click(add5);
$("#sub1").click(sub1);
$("#zero").click(zero);










});