<!-- 
  author: Matthew Hird
  csciId: hirdm
  date: Mar 28, 2019
  links: 
-->
<?php
  session_start();
  require_once('../resources/config.php');

  $author_name = "Matthew Hird";
  $page_title = "Login Page - $siteTitle";
  $h1_header = "Login Page";

  $stylesheets[] = 'greysiteStyle.css';

  require(TEMPLATES."head.php");
?>
  
<?php require(TEMPLATES."header.php"); ?>

<?php require(TEMPLATES."loginWidget.php"); ?>

<?php require(TEMPLATES."footer.php"); ?>
