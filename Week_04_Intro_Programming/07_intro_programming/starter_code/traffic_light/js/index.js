//On Click for the buttons. We want the buttons to change the colors
// of the lights 
//each of these statements runs illuminate which clears and then
//turns on the correct light 
document.getElementById('stopButton').onclick = illuminateRed;
document.getElementById('slowButton').onclick = illuminateYellow;
document.getElementById('goButton').onclick = illuminateGreen;


//Build the functions 
//clearLights - this clears ALL lights to black so only one color shows
function clearLights() {
  document.getElementById('stopLight').style.backgroundColor = "black";
  document.getElementById('slowLight').style.backgroundColor = "black";
  document.getElementById('goLight').style.backgroundColor = "black";
}

//illuminateRed
//This function runs clearLights and then changes the stoplight to red
function illuminateRed() {
  clearLights();
  document.getElementById('stopLight').style.backgroundColor = "red";
}


//illuminateYellow
//This function runs clearLights and then changes the slowlight to yellow
function illuminateYellow() {
  clearLights();
  document.getElementById('slowLight').style.backgroundColor = "yellow";
}

//illuminateGreen
//This function runs clearLights and then changes the golight to green 
function illuminateGreen() {
  clearLights();
  document.getElementById('goLight').style.backgroundColor = "green";
}