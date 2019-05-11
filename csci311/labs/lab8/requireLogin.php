<?php
// author: Matthew Hird
// csciId: hirdm
// date: Mar 8, 2019
// links: 

  session_start();
  // $_SESSION['return_to_page'] stores the current page URI so the current page
  // loads after successful login.
  $_SESSION['return_to_page'] = $_SERVER['REQUEST_URI'];
  if(!isset($_SESSION['user'])){
    header("location: Login.php");  
    exit;
  }

  // If __DIR__.'/requireLogin.php' is accessed directly in the inital http 
  // request, index.php will load instead.
  if ($_SERVER['DOCUMENT_ROOT'] . $_SESSION['return_to_page'] == __FILE__) {
    header("location: index.php");  
  }
?>