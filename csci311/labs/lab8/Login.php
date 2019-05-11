<!-- 
  author: Matthew Hird
  csciId: hirdm
  date: Mar 8, 2019
  links: 
-->
<?php
  session_start();
  $err ="";
  $userList = array("admin" => "1234");
  if(isset($_POST['user_name'])){
    $username = $_POST['user_name'];
    $password = $_POST['password'];
    if ($username == '') {
      $err .= "Username is a required field<br />";
    } 
    if ($password == '') {
      $err .= "Password is a required field<br />";
    } else {
      //check against our valid passwords
      if(array_key_exists($username, $userList)){
        if($userList[$username] == $password){
          //set the 'user' session variable if valid username and password 
          $_SESSION['user'] = $username;

          $return_to_page = "index.php";
          if (isset($_SESSION['return_to_page'])) {
            $return_to_page = $_SESSION['return_to_page'];
          }
          // Returns to page requested before login 
          // or index.php if $_SESSION['return_to_page'] is not set 
          header("location: $return_to_page");
          exit();
        }else{
          // Invalid password
          $err = "Sorry the username or password is invalid";
        }
      }else{
        // Invalid username
        $err = "Sorry the username or password is invalid";
      }
    }
  }
?>
<?php
  $author_name = "Matthew Hird";
  $page_title = "Login Page - Matt's CSCI 311";
  $h1_header = "Login Page";

  require("head.php");
?>
  <body>
    <main>
      <h1><?php echo $h1_header; ?></h1>
      <form method="post" action="<?php echo $_SERVER['PHP_SELF']; ?>">
        <?php
          if($err){
            echo "<span>$err</span><br />";
          }
        ?>
        <fieldset>
          <label for="user_name">Username:</label>
          <input id="user_name" name="user_name" type="text" placeholder="username" pattern="[A-Za-z0-9]{4,}" 
              title="Username must be at least 4 characters">
        </fieldset>

        <fieldset>
          <label for="password">Password:</label>
          <input id="password" name="password" type="password">
        </fieldset>

        <fieldset>
          <input type="submit" value="Login">
        </fieldset>
      </form>
    </main>
  </body>
</html>
