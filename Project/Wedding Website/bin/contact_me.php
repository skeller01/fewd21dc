<?php
// check if fields passed are empty
if(empty($_POST['name'])  		||
   empty($_POST['email']) 		||
   empty($_POST['additional'])	||
   empty($_POST['street'])	||
   empty($_POST['city'])	||
   empty($_POST['state'])	||
   empty($_POST['zip'])	||
   !filter_var($_POST['email'],FILTER_VALIDATE_EMAIL))
   {
	echo "No arguments Provided!";
	return false;
   }
	
$name = $_POST['name'];
$email_address = $_POST['email'];
$message = $_POST['additional'];
$street = $_POST['street'];
$city = $_POST['city'];
$state = $_POST['state'];
$zip = $_POST['zip'];
	
// create email body and send it	
$to = 'skeller01@gmail.com'; // put your email
$email_subject = "Contact form submitted by:  $name";
$email_body = "You have received a new message. \n\n".
				  " Here are the details:\n \nName: $name \n ".
				  " Address details:\n \nStreet: $street \n ".
				  " Address details:\n \nCity: $city \n ".
				  " Address details:\n \nState: $state \n ".
				  " Address details:\n \nZip: $zip \n ".
				  "Email: $email_address\n Message \n $message";
$headers = "From: \n";
$headers .= "Reply-To: $email_address";	
mail($to,$email_subject,$email_body,$headers);
return true;			
?>