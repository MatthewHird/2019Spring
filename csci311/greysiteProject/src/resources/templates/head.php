<?php 
// author: Matthew Hird
// csciId: hirdm
// date: Mar 16, 2019
// links: 
echo "<!DOCTYPE html>
<html xmlns='http://www.w3.org/1999/xhtml' lang='en' class='no-js'>
  <head>
    <meta charset='utf-8' />
    <meta http-equiv='x-ua-compatible' content='ie=edge'>
    <meta name='viewport' content='width=device-width, initial-scale=1.0' />
    <title>$page_title</title>";
   
  if (isset($stylesheets)) {
      $stylesheets = array_unique ($stylesheets);
      foreach ($stylesheets as $key => $value) {
        echo "<link rel='stylesheet' type='text/css' href='css/$value' />";
      }
    }
  
  echo "
  </head>
  <body>";
?>