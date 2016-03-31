$(document).ready(function(){


	//Prevent the links from moving around the page
	$("a").click(function(event){
	    event.preventDefault();
	});


	//Initiate the toggle for the links with a click
	//main menu 
	$(".readmore").click(content_toggle_main);
	//side bar menu
	$(".learnmore").click(content_toggle_sidebar);

	//revert for main menu 
	$(".readless").click(content_toggle_offmain);


	//Create the content_toggle function

	function content_toggle_main(){
		//$("#show-this-on-click").toggle();
		$("#show-this-on-click").slideDown();
		$(".readmore").hide();
		$(".readless").toggle();
				
		

	}

		function content_toggle_offmain(){
		//$("#show-this-on-click").toggle();
		//$(".readless").toggle();
		$("#show-this-on-click").slideUp(function(){
				$(".readless").toggle();
				$(".readmore").show();

		});
		

	}

	// function content_toggle_sidebar(){
		function content_toggle_sidebar(){
			$("#learnmoretext").slideDown();
			$(".learnmore").hide();
			

	}




















});