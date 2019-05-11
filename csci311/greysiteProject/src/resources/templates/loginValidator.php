<?php 
// author: Matthew Hird
// csciId: hirdm
// date: Apr 1, 2019
// links: 
 
function checkPassword($testPassword, $realHash) {
  if($realHash === crypt($testPassword, $realHash)) {
    return true;
  } else {
    return false;
  }
}

function userLogin($dblogin, $username, $password) {

  $query1 = "SELECT L.password_hash FROM  UserAccount UA JOIN Login L ON UA.user_id = L.user_id WHERE UA.username = ?";

  try {
    $conn = dbconnect($dblogin);
    
    $stmt1 = $conn->prepare($query1);

    $stmt1->bindParam(1, $username);
    $stmt1->execute();
    $result = $stmt1->fetch();


    if ($result) {
      $hashedPassword = $result['password_hash'];
      $conn = null;
      if (checkPassword($password, $hashedPassword)) {
        return true;
      } else {
        return false;
      }   
    } else {
      checkPassword($password, '$2y$10$sYYt3nIonsEEwSQHuanEiiWYhgd9fKli56Jf8eEwp20ScwHdqzU6a');
      return false;
    }

  } catch(PDOException $e) {
    print "Error!" . $e->getMessage() . "<br/>";
    $conn = null;
    return false;
  }
  $conn = null;

  return true;
}


if(isset($_POST['username'])){
  $username = $_POST['username'];
  $password = $_POST['password'];
  if ($username == '') {
    $err .= "<i class='fi-alert'></i> Username is a required field<br />";
  } 
  if ($password == '') {
    $err .= "<i class='fi-alert'></i> Password is a required field<br />";
  } 

  if (!$err) {
    //check against our valid passwords
    if(userLogin($dblogin, $username, $password) == true){
      //set the 'user' session variable if valid username and password 
      $_SESSION['user'] = $username;

      // Returns to page requested before login 
      // or index.php if $_SESSION['return_to_page'] is not set 
      header("location: $return_to_page");
      exit();
    } else {
      // Invalid password
      $err = "<i class='fi-alert'></i> Sorry the username or password is invalid<br />";
    }
  }
}

?>