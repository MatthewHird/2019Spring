<?php 
  // author: Matthew Hird
  // csciId: hirdm
  // date: Feb 15, 2019
  // links: 
  $user;
  $animal1;
  $animal2;
  $type;
  $col;
  $err;

  if (isset($_POST)) {
    $user = trim($_POST['name']);
    $animal1 = trim($_POST['animal1']);
    $animal2 = trim($_POST['animal2']);
    $type = trim($_POST['element']);
    $col = trim($_POST['colour']);

    if ($user === "") {
      $err = "Your Name field is blank";
    } else if ($animal1 === "") {
      $err = "Animal One field is blank";
    } else if ($animal2 === "") {
      $err = "Animal Two field is blank";
    } else if ($type === "") {
      $err = "Creature Element field is blank";
    } else if ($col === "") {
      $err = "Colour field is blank";
    }  
  } 
?>

<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
  <head>
    <meta charset="utf-8"/>
    <title>Uber Animal Doom Page</title>
    <link rel="stylesheet" type="text/css" href="myStyles.css"/>
  </head>
  <body style="background-color: <?php echo $col; ?>">
    <?php   
      if ($err) {
        echo $err;
        return;
      } else {
    ?>
      <h1><?php echo $user; ?>, you created the following amazing beast:</h1>
      <p style="font-size: large; text-align: center;">Name:
        <?php echo $animal1." ".$animal2." of ".$type ?></p>
      <a href="./lab5Form.html"><- Back</a>
    <?php } ?>
  </body>
</html>