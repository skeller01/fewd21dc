//set up document 
$(document).ready(function() {
  //on click we want to toggle the slideDown function
  $('header nav ul li:first-child').click(function(){
    $(this).parent().toggleClass('slideDown');
  });
});



// Homework solution notes 
// step 1: Create a js file and app.js in the working directory 
// step 2: Add the script tags to the html document for the toggle down feature and to connect the javascript to the page 
// step 3:Add html divs for the three line pull down 
// step 4: add css for the lines 
// step 5: make sure the container class throughout html is sensitive to screen size with percents and a max-width
// step 6: Make sure the blog has the same "lines" and the connection to jquery on that page too