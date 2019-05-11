<?php 

function password_encrypt($password){
    $hash_format = "$2y$10$";
    $salt_length = 22;
    $salt = generate_salt($salt_length);
    $format_and_salt = $hash_format.$salt;
    $hash = crypt($password, $format_and_salt);
   
    return $hash;
  } 
  
  function generate_salt($length){
    //generate pseudo random string (good enough)
    //returns 32 characters
    $unique_random_string = md5(uniqid(mt_rand(), true));
    
    //convert it to base 64 (valid chars are [a-zA-Z0-0./] )
    $base64_string = base64_encode($unique_random_string);
    
    //remove the '+' characters, just replace with '.'
    $modified_base64_string = str_replace('+', '.', $base64_string);
    
    //truncate off just what we need
    $salt = substr($modified_base64_string, 0, $length);
    
    return $salt;
  }

echo password_encrypt("password");
die;

// <?php require("~/Desktop/passHashGenerator.php"); ?>
 

?>