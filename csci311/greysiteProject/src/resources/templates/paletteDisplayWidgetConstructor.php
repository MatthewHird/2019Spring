<?php  
// author: Matthew Hird
// csciId: hirdm
// date: Mar 17, 2019
// links: 

function paletteDisplayWidget($paletteName, $paletteAuthor, $colourIndexArray) {
  $pdwString = sprintf("<div class='paletteDisplayWidget'><div class='pdw-colours'>");   
    
    foreach ($colourIndexArray as $index => $colour) {
      $pdwString .= sprintf("<div class='pdw-colour pdw-colour-0' style='background-color: "
        ."rgb(%s, %s, %s);'></div>", $colour['red'], $colour['green'], $colour['blue']);
    }
    
  $pdwString .= sprintf("</div><div class='pdw-label'><div class='pdw-name'>%s</div>"
      ."<div class='pdw-author'>by %s</div></div></div>", $paletteName, $paletteAuthor);

  return $pdwString;
}

?>
