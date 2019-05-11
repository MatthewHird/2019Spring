<?php
// author: Matthew Hird
// csciId: hirdm
// date: Mar 31, 2019
// links: 

  session_start();
  // $_SESSION['return_to_page'] stores the current page URI so the current page loads after successful login.
  $_SESSION['return_to_page'] = $_SERVER['REQUEST_URI'];
  if (isset($_SESSION['user'])) {
    $user = $_SESSION['user'];
  } 
?>