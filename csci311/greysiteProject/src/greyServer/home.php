<!-- 
  author: Matthew Hird
  csciId: hirdm
  date: Apr 1, 2019
  links: 
-->
<?php
  require_once('../resources/config.php');
  require_once(TEMPLATES."checkLogin.php");

  $author_name = "Matthew Hird";
  $page_title = "Home - $siteTitle";
  $h1_header = "Explore Grey Palettes";

  $stylesheets[] = 'greysiteStyle.css';

  require(TEMPLATES."head.php");
?>
  
<?php require(TEMPLATES."header.php"); ?>

<?php 
  require(TEMPLATES."allPalettesWidgetConstructor.php"); 
  echo "<h2>All Palettes <small>sorted by modification date</small></h2>".allPalettesWidget($dblogin);
?>

<?php require(TEMPLATES."footer.php"); ?>
