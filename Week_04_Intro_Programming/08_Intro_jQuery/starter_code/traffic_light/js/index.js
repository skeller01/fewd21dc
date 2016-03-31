//Implement the red light using jQuery. Don't forget to add the script tags.

$( document ).ready(function() {
    // console.log( "ready!" );
    //on events for stop, go and slow
    	$('#stopButton').click(stop);
    	$('#goButton').click(go);
    	$('#slowButton').click(slow);

    //functions for clear, stop, go and slow 

    //build clear button 
    	function clear(){
    		// $("#stopLight").css("background-color","black");
    		// $("#goLight").css("background-color","black");
    		// $("#slowLight").css("background-color","black");

    		$(".bulb").css("background-color","black")
    	}

    //build stop
	    function stop(){
	    	clear();
	    	$("#stopLight").css("background-color","red");
	    }

    //build go 
	    function go(){
	    	clear();
	    	$("#goLight").css("background-color","green");
	    }

    //build slow 
	    function slow(){
	    	clear();
	    	$("#slowLight").css("background-color","yellow");
	    }




});