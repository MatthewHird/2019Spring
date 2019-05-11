<!-- 
  author: Matthew Hird
  csciId: hirdm
  date: Jan 21, 2019
  links: 
-->

<?php
    $page_title = "Matt's Lab 5 Home";
    $pageKind = "dynamic";
    require("front.php");
?>

    <div class="content">
    <h2>Welcome to my <?php echo $pageKind ?> html page for Lab 5</h2>
    <p>
    In this lab we will demonstrate how to build a web page using php.  You will
    duplicate this page, splitting out the portions that are shared between
    pages from the content that is specific to this page.
    </p>
    <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod 
    tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, 
    quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo 
    consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse 
    cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat 
    non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. </p>
    </div>
    <?php require("back.php") ?>