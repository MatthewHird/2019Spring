 <?php
// author: Matthew Hird
// csciId: hirdm
// date: Mar 16, 2019
// links: 

  $siteTitle = '15 Shades of Grey';

  define('ROOT_DIR', '../');
  define('TEMPLATES', ROOT_DIR."resources/templates/");

  $stylesheets = array('frameworks/foundation.min.css', 'frameworks/foundation-icons.css');
  $jsImports = array('frameworks/jquery.js', 'frameworks/what-input.js',
                     'frameworks/foundation.min.js', 'greysite.js');
  
  $dblogin = array('host' => 'localhost', 'user' => 'xxxxxx',
                   'password' => 'xxxxxx', 'database' => 'xxxxxx');

  function dbconnect($dblogin) {
    return new PDO("mysql:host=".$dblogin['host'].";dbname=".$dblogin['database'], 
        $dblogin['user'], $dblogin['password']);
  }
?>
