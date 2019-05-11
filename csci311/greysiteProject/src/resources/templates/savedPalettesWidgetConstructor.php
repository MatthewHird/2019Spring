<?php
// author: Matthew Hird
// csciId: hirdm
// date: Apr 1, 2019
// links: 

require_once(TEMPLATES."paletteDisplayWidgetConstructor.php");

function savedPalettesWidget($dblogin, $user, $saveType=false) {
  $query1 = "SELECT P.palette_id, P.name, P.user_id FROM Palette P JOIN SavedPalette S ON P.palette_id = S.palette_id JOIN UserAccount UA ON S.user_id = UA.user_id";

  $query2 = "SELECT red, green, blue FROM Colour C JOIN PaletteColour PC ON C.colour_id = PC.colour_id WHERE PC.palette_id = ? ORDER BY position_index";

  $query3 = "SELECT username FROM UserAccount WHERE user_id = ?";

  if ($saveType) {
    $query1 .= " WHERE P.user_id != UA.user_id AND UA.username = ?";
  } else {
    $query1 .= " WHERE P.user_id = UA.user_id AND UA.username = ?";
  }

  $spwString = "";

  try {
    $conn = dbconnect($dblogin);
    
    $stmt1 = $conn->prepare($query1);

    $stmt2 = $conn->prepare($query2);

    $stmt3 = $conn->prepare($query3);

    $stmt1->bindParam(1, $user);
    $stmt1->execute();
    $savedPalettes = $stmt1->fetchAll();

    foreach ($savedPalettes as $key => $palette) {
      $stmt2->bindParam(1, $palette['palette_id']);
      $stmt2->execute();
      $paletteColours = $stmt2->fetchAll();

      $stmt3->bindParam(1, $palette['user_id']);
      $stmt3->execute();
      $author = $stmt3->fetchColumn();
      
      $spwString .= paletteDisplayWidget($palette['name'], $author, $paletteColours);
    }

  }catch(PDOException $e){
    print "Error!" . $e->getMessage() . "<br/>";
  }
  $conn = null;

  return $spwString;
}

?>