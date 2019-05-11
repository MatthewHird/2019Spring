<?php
// author: Matthew Hird
// csciId: hirdm
// date: Mar 6, 2019
// links:
  session_start();
  $_SESSION = array();
  session_destroy();
  header("location: ./index.php");
  exit;
?>