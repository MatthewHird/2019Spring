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
  $page_title = "Create Palette - $siteTitle";
  $h1_header = "Create Your Palette";

  $stylesheets[] = 'greysiteStyle.css';

  require(TEMPLATES."head.php");
?>

<?php require(TEMPLATES."header.php"); ?>


<?php require(TEMPLATES."createPaletteWidget.php"); ?>


<?php require(TEMPLATES."footer.php"); ?>
