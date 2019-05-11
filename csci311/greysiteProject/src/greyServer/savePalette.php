<?php 
// author: Matthew Hird
// csciId: hirdm
// date: Apr 1, 2019
// links: 

  require_once('../resources/config.php');
  require_once(TEMPLATES."checkLogin.php");


  function savePalette($dblogin, $username, $pname, $pColourArray) {

    $query1 = "SELECT username, user_id FROM  UserAccount WHERE username = ?";

    $query2 = "SELECT colour_id FROM Colour WHERE red = ? AND green = ? AND blue = ?";

    $query3 = "SELECT MAX(palette_id + 1) AS newPid FROM Palette";

    $query4 = "INSERT INTO Palette (palette_id, name, user_id, type, size, creation_date, modification_date) SELECT ?, ?, ?, 'simple', 5, CURRENT_DATE, CURRENT_DATE";

    $query5 = "INSERT INTO SavedPalette (palette_id, user_id) SELECT ?, ?";

    $query6 = "INSERT INTO PaletteColour (palette_id, colour_id, position_index) SELECT ?, ?, ?";

    try {
      $conn = dbconnect($dblogin);
      
      $stmt1 = $conn->prepare($query1);
      $stmt2 = $conn->prepare($query2);
      $stmt3 = $conn->prepare($query3);
      $stmt4 = $conn->prepare($query4);
      $stmt5 = $conn->prepare($query5);
      $stmt6 = $conn->prepare($query6);


      $stmt1->bindParam(1, $username);
      $stmt1->execute();
      $result1 = $stmt1->fetch();

      if (!$result1) {
        return "User not in system<br />";
      } 

      $userId = $result1['user_id'];

      $colIdSet = array();

      for ($i=0; $i < 5; $i++) { 
        $curColour = $pColourArray[$i];
        $stmt2->bindParam(1, $curColour[0]);
        $stmt2->bindParam(2, $curColour[1]);
        $stmt2->bindParam(3, $curColour[2]);
        $stmt2->execute();
        $result2 = $stmt2->fetch(); 

        if (!$result2) {
          return "Colour not in system<br />";
        } 
        
        $colIdSet[] = $result2['colour_id'];
      }

      $stmt3->execute();
      $result3 = $stmt3->fetch();
      $paletteId = $result3['newPid'];
      
      $stmt4->bindParam(1, $paletteId);
      $stmt4->bindParam(2, $pname);
      $stmt4->bindParam(3, $userId);
      $stmt4->execute();

      $stmt5->bindParam(1, $paletteId);
      $stmt5->bindParam(2, $userId);
      $stmt5->execute();

      $stmt6->bindParam(1, $paletteId);
      for ($i=0; $i < 5; $i++) { 
        $stmt6->bindParam(2, $colIdSet[$i]);
        $stmt6->bindParam(3, $i);
        $stmt6->execute();
      }


    } catch(PDOException $e) {
      $conn = null;
      return "Database error<br />";
    }
    $conn = null;

    return "ok";
  }


  $err = "";

  if (isset($_POST)) {
    if (!isset($user)) {
      $err .= "Must be logged in to save palette<br />";
    } else if (trim($_POST['pname']) == "") {
      $err .= "Palette Name is a required field<br />";
    } else {
      $paletteName = trim($_POST['pname']);
      $paletteColours = array();
      for ($i=0; $i < 5; $i++) { 
        $paletteColours[] = array($_POST['c'.($i + 1).'r'], $_POST['c'.($i + 1).'g'], $_POST['c'.($i + 1).'b']);

        if ($pColour[0] != $pColour[1] || $pColour[0] != $pColour[2]) {
          $err .= "All palette colours must have equal red, green and blue values<br />";
          echo $err;
          exit;
        }
      }
      $isError = savePalette($dblogin, $user, $paletteName, $paletteColours);
      if ($isError == "ok") {
        echo "Palette saved successfully<br />";
      } else {
        echo $isError;
      }
    }

  } else {
    $err .= "No palette to save<br />";
  }

  if (trim($err)) {
    $res = $err;
  }

  echo $res;
  exit; 
?>