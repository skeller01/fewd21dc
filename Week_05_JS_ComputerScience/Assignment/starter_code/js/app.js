//wait for the DOM elements to load before executing
$(document).ready(function () {

    // Create a function that runs whenever the submit button is clicked
        $("#submit-btn").click(function(e){
        //prevent the submit button from refreshing the page
        e.preventDefault();

        //Create a variable called city input and get the value of the #city-type input
        var cityInput=$("#city-type").val();
        console.log(cityInput);

        //Correct for capitalization
        cityInput=cityInput.toLowerCase();
        cityInput=$.trim(cityInput);
        console.log(cityInput);
        // if the user inputs ny change the CSS class to nyc'
         if(cityInput=="new york"||cityInput=="new york city"||cityInput=="nyc"){$("body").attr("class","nyc");$("#city-type").val("");}
        // if the user inputs sanfrancisco change the CSS class to sf
        else if(cityInput=="san francisco"||cityInput=="bay area"||cityInput=="sf"){$("body").attr("class","sf");$("#city-type").val("");}
        // if the user inputs lax change the CSS class to 'la'
        else if(cityInput=="los angeles"||cityInput=="la"||cityInput=="lax"){$("body").attr("class","la");$("#city-type").val("");}
        // if the user inputs austin change the CSS class to 'austin'
        else if(cityInput=="austin"||cityInput=="atx"){$("body").attr("class","austin");$("#city-type").val("");}
        // if the user inputs sydney change the CSS class to 'sydney'
        else if(cityInput=="sydney"||cityInput=="syd"){$("body").attr("class","sydney");$("#city-type").val("");}
        

});

   

    

});//end of document ready function