$(document).ready(function(){


	//create array for images
	var images=["images/animal2.jpg","images/animal3.jpg","images/animal4.jpg","images/animal5.jpg","images/animal6.jpg"];
	//create variables for counting including max
	var i = 0; 
	var maxImages=images.length-1; 


	//create function for back button
	function previousImage(){
		//if var >0 increment back else stay at max images, else increment image
		if(i>0){
			i--;
		}else{
			i = maxImages;
		}
		changeImage(i);
		console.log(i);
	}

	//create function for next button 
	function nextImage(){
		//if increment var < 0 then maxImages increment forward else start at 0 and change image 
		if(i<maxImages){
			i++;
		}else{
			i=0;
		}
		changeImage(i);
		console.log(i);
	}

	//create a function to change the image 
	function changeImage(x){
		//Update image source to the current image
		$("#carousel-image").attr("src",images[x]);
	}




	//event listeners 
	//could wrap just this section in the document ready
	$("#next").on("click",nextImage);
	$("#back").on("click",previousImage);










});