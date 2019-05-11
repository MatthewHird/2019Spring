 <?php
// author: Matthew Hird
// csciId: hirdm
// date: April 13, 2019

// ini_set('display_errors', 1);
// error_reporting(E_ERROR | E_WARNING | E_PARSE); 

$siteTitle = 'X-Wing Project';
$siteIcon25x25 = 'img/xwing-icon-25x25.png';
$siteIcon38x38 = 'img/xwing-icon-38x38.png';
$siteIcon50x50 = 'img/xwing-icon-50x50.png';
$siteIcon75x75 = 'img/xwing-icon-75x75.png';

define('ROOT_DIR', '../');
define('TEMPLATES', ROOT_DIR."csci370Resources/templates/");
define('GEN_TEMPLATES', TEMPLATES."generic/");
define('WIDGETS', TEMPLATES."xwingWidgets/");
define('LAYOUTS', TEMPLATES."layouts/");

$stylesheets = array('frameworks/foundation.min.css', 'frameworks/foundation-icons.css');
$jsImports = array('frameworks/jquery.js', 'frameworks/what-input.js',
                                     'frameworks/foundation.min.js', 'xwingProject.js');

define('DB_LOGIN', array('host' => 'localhost', 'user' => 'xxxxxx',
                                 'password' => 'xxxxxx', 'database' => 'xxxxxx'));

function dbconnect() {
        return new PDO("mysql:host=".DB_LOGIN['host'].";dbname=".DB_LOGIN['database'], 
                DB_LOGIN['user'], DB_LOGIN['password']);
}

?>
