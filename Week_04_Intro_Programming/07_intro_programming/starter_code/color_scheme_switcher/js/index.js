//On click white button set to colors white
document.getElementById("grayButton").onclick=switch_to_white;

//On click grey button set colors grey 
document.getElementById("whiteButton").onclick=switch_to_grey;



// function to make colors white
function switch_to_white(){

		// change the background
		document.body.style.backgroundColor="white";
		//change text colors
		document.body.style.color="black";

}


//function to make colors grey
function switch_to_grey(){


		//change background 
		document.body.style.backgroundColor="grey";
		//change text colors  
		document.body.style.color="white";

}



