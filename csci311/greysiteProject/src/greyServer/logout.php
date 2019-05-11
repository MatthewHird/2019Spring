<!-- 
  author: Matthew Hird
  csciId: hirdm
  date: Mar 16, 2019
  links:
-->
<?php
  session_start();
  $_SESSION = array();
  session_destroy();
  header("location: index.php");
  exit;
?>