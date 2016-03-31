//CLues us in the javascript file is properly connected
jQuery( document ).ready(function() {
    // console.log( "ready!" );
    

//define event on gray butitchton to switch colors
jQuery("#greenButton").click(switchGreen);
//define event on white button to switch colors
jQuery("#whiteButton").click(switchWhite);

jQuery("#purpleButton").click(switchPurple);

//Define function to switch background and text to gray style
function switchGreen(){
	jQuery("body").css("background-color","green");
    	
    }

//Define funtion to switch background and text to white style
function switchWhite(){
	jQuery("body").css("background-color","white");
    	
    }
function switchPurple(){
	jQuery("body").css("background-color","purple");
    	
    }






})








// document.getElementById('grayButton').onclick = switchGray;
// document.getElementById('whiteButton').onclick = switchWhite;

// function switchGray() {
//   document.body.style.backgroundColor = 'gray';
//  document.body.style.color = 'white';
// }

// function switchWhite() {
//   document.body.style.backgroundColor = 'white';
//   document.body.style.color = 'black';
// }


