<!-- 
  author: Matthew Hird
  csciId: hirdm
  date: Mar 16, 2019
  links: 
-->
<?php
  require_once('../resources/config.php');
  require_once(TEMPLATES."checkLogin.php");

  if (!isset($user)) {
    header("Location: login.php");
    exit;
  } 

  $author_name = "Matthew Hird";
  $page_title = "User Profile - $siteTitle";
  $h1_header = "$user Profile";
  
  $stylesheets[] = 'greysiteStyle.css';

  require(TEMPLATES."head.php");
?>

<?php require(TEMPLATES."header.php"); ?>

<?php 
  require_once(TEMPLATES."savedPalettesWidgetConstructor.php");
  echo "<h2>Saved Palettes</h2>".savedPalettesWidget($dblogin, $user);
  echo "<h2>Favourite Palettes</h2>".savedPalettesWidget($dblogin, $user, true);
?>

<?php require(TEMPLATES."footer.php"); ?>
