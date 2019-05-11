<?php
// author: Matthew Hird
// csciId: hirdm
// date: Apr 1, 2019
// links: 
  $return_to_page = "index.php";
  if (isset($_SESSION['return_to_page'])) {
    $return_to_page = $_SESSION['return_to_page'];
  }

  $err ="";
  
  require(TEMPLATES."loginValidator.php");
?>

<form data-abide novalidate method="post" action="<?php echo $_SERVER['PHP_SELF']; ?>">
  <?php
    if($err){
      echo "<div data-abide-error class='alert callout' style='display: block;''>
    <p> $err</p>
  </div>";
    }
  ?>

  <div class="loginWidget">   
    <div class="lw-outer">
      <div class="lw-inner">
        <div class="lw-logo">
          <img src="img/greysiteLogo75x75.png" <?php echo "alt='$siteTitle logo'"; ?>>
        </div>

        <div>  
          
          <div id="lwUsername">
            <label for="lwUsernameInput" class="lw-text">Username:</label>
            <input id="lwUsernameInput" type="text" name="username" 
                placeholder="username" aria-describedby="lwUsernameHelp" 
                aria-errormessage="lwInvalidUsername" required pattern="[A-Za-z][A-Za-z0-9]*" 
                <?php if (isset($_POST['username'])) { echo "value='".$_POST['username']."'"; } ?>>
            <span class="form-error" id="lwInvalidUsername">Username is a required field.</span>
            <p class="help-text show-for-sr" id="lwUsernameHelp">Please enter your <?php echo $siteTitle ?> username</p>
          </div>

          <div id="lwPassword">
            <label for="lwPasswordInput" class="lw-text">Password:</label>
            <input id="lwPasswordInput" type="password" name="password" placeholder="password" 
                aria-describedby="lwPasswordHelp" aria-errormessage="lwInvalidPassword" required>
            <span class="form-error" id="lwInvalidPassword">Password is a required field.</span>
            <p class="help-text show-for-sr" id="lwPasswordHelp">Please enter your <?php echo $siteTitle ?> password</p>
          </div>


          <input id="lwLoginButton" class="button" type="submit" value="Login" aria-describedby="lwLoginHelp">
          <p class="help-text show-for-sr" id="lwLoginHelp">Click to validate login</p>
            
          <div>
            <a class="lw-text" <?php echo "href='$return_to_page'"; ?>>Return to previous page</a>
          </div>
        </div>
      </div>
    </div>
  </div>
</form>
