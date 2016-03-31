// Create array with the city abbrviations 
var cities = ["NYC", "SF", "LA", "ATX", "SYD"];

//load in the document objects 
$(document).ready(function() {
  //Run a loop through the city array that
  //automatically adds options to the form's pull down
  //note: this could have been done in the html  
  for(i=0;i<cities.length;i++) {
    $('#city-type').append('<option value="' + cities[i] + '">' + cities[i] + '</option>');
  }

//When the form is clicked and changed, the function should change the background
//the on change button listens for the drop down to be changed
  $('form').on('change', '#city-type',function(){
    //first, store the city being selected for comparison purposes 
    var city = $('#city-type').val();
    //if NYC then give body the class nyc in css
    if(city == 'NYC') {
      $('body').attr('class','nyc');
    }
    //if SF then give body the class nyc in css
    else if (city == 'SF') {
      $('body').attr('class','sf');
    }
    //if LA then give body the class nyc in css
    else if (city == 'LA') {
      $('body').attr('class','la');
    }
    //if ATX then give body the class nyc in css
    else if (city == 'ATX') {
      $('body').attr('class','austin');
    }
    //if SYD then give body the class nyc in css
    else if (city == 'SYD') {
      $('body').attr('class','sydney');
    }
  });
});