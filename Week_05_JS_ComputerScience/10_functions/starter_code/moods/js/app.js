//wait for the DOM elements to load before executing
$(document).ready(function () {

    // Create a function that runs whenever the submit button is clicked
        $("#submit-btn").click(function(e){
        //prevent the submit button from refreshing the page
        e.preventDefault();

        //Create a variable called moodvalue and get the value of the #mood input
        var moodInput=$("#mood").val();
        console.log(moodInput);

        //Correct for capitalization
        moodInput=moodInput.toLowerCase();
        console.log(moodInput);
        // if the user inputs excited / ecstatic / fantastic change the CSS class to 'excited'
         if(moodInput=="excited"||moodInput=="ecstatic"||moodInput=="fantastic"){$(".moodring>div").attr("class","excited");}
        // if the user inputs happy/good/great change the CSS class to 'happy'
        else if(moodInput=="happy"||moodInput=="good"){$(".moodring>div").attr("class","happy");}
        // if the user inputs bad/angry change the CSS class to 'bad'
        else if(moodInput=="bad"||moodInput=="angry"){$(".moodring>div").attr("class","bad");}
});

    // Listen for user interaction on the submit button.

    

});//end of document ready function