<!-- 
  author: Matthew Hird
  csciId: hirdm
  date: Mar 9, 2019
  links: 
-->
<?php
  require_once("requireLogin.php");

  $author_name = "Matthew Hird";
  $page_title = "Lab 8 Bonus - Matt's CSCI 311";
  $h1_header = "Lab 8 Bonus";

  require("head.php");
?>

<?php 

  function draw_circle($xpos, $ypos, $radius, $colour) {
    return "
        context.beginPath();
        context.arc($xpos, $ypos, $radius, 0, 2 * Math.PI);
        context.fillStyle = '$colour';
        context.fill();
        context.strokeStyle = 'black';
        context.lineWidth = 3;
        context.stroke(); ";
  }

  function draw_rectangle($xpos, $ypos, $width, $height, $colour) {
    $xoffset = $xpos - $width / 2;
    $yoffset = $ypos - $height / 2;
    return "
        context.beginPath();
        context.rect($xoffset, $yoffset, $width, $height);
        context.fillStyle = '$colour';
        context.fill();
        context.strokeStyle = 'black';
        context.lineWidth = 3;
        context.stroke(); ";
  }

  function draw_square($xpos, $ypos, $length, $colour) {
    $xoffset = $xpos - $length / 2;
    $yoffset = $xpos - $length / 2;
    return "
        context.beginPath();
        context.rect($xoffset, $yoffset, $length, $length);
        context.fillStyle = '$colour';
        context.fill();
        context.strokeStyle = 'black';
        context.lineWidth = 3;
        context.stroke(); ";
  }
?>

<?php
  $err;
  $drawing = "";

  if (isset($_SESSION['drawing'])) {
    $drawing = $_SESSION['drawing'];
  }

  if (isset($_POST['clear'])) {
    $_SESSION['drawing'] = '';
    $drawing = '';
  } else if (!empty($_POST)) {
    
    if ($_POST['shape'] == "blank") {
      $err .= "Shape is a required field <br />";
    }

    if ($_POST['x_coordinate'] == '') {
      $err .= "X Coordinate is a required field <br />";
    } else if ($_POST['x_coordinate'] > 300 || $_POST['x_coordinate'] < -300) {
      $err .= "X Coordinate must be between -300 and 300 <br />";
    }

    if ($_POST['y_coordinate'] == '') {
      $err .= "Y Coordinate is a required field <br />";
    } else if ($_POST['y_coordinate'] > 200 || $_POST['y_coordinate'] < -200) {
      $err .= "Y Coordinate must be between -200 and 200 <br />";
    }

    if (isset($_POST['radius'])) {
      if ($_POST['radius'] == '') {
        $err .= "Radius is a required field <br />";
      } else if ($_POST['radius'] > 200 || $_POST['radius'] < 5) {
        $err .= "Radius must be between 5 and 200 <br />";
      }
    }

    if (isset($_POST['length'])) {
      if ($_POST['length'] == '') {
        $err .= "Length is a required field <br />";
      } else if ($_POST['length'] > 400 || $_POST['length'] < 5) {
        $err .= "Length must be between 5 and 400 <br />";
      }
    }

    if (isset($_POST['width'])) {
      if ($_POST['width'] == '') {
        $err .= "Width is a required field <br />";
      } else if ($_POST['width'] > 600 || $_POST['width'] < 5) {
        $err .= "Width must be between 5 and 600 <br />";
      }
    }

    if (isset($_POST['height'])) {
      if ($_POST['height'] == '') {
        $err .= "Height is a required field <br />";
      } else if ($_POST['height'] > 400 || $_POST['height'] < 5) {
        $err .= "Height must be between 5 and 400 <br />";
      }
    }

    if (!$_POST['colour']) {
      $err .= "Colour is a required field <br />";
    }

    if (!isset($err)) {
      if ($_POST['shape'] == 'circle') {
        $drawing .= draw_circle($_POST['x_coordinate'], $_POST['y_coordinate'], $_POST['radius'], $_POST['colour']);
      } else if ($_POST['shape'] == 'rectangle') {
        $drawing .= draw_rectangle($_POST['x_coordinate'], $_POST['y_coordinate'], $_POST['width'], $_POST['height'], $_POST['colour']);
      } else if ($_POST['shape'] == 'square') {
        $drawing .= draw_square($_POST['x_coordinate'], $_POST['y_coordinate'], $_POST['length'], $_POST['colour']);
      }

      $_SESSION['drawing'] = $drawing;
    }
  }
?>
  <body>

    <?php require("header.php"); ?>

    <main>
      <form action="Lab8Bonus.php" method="POST">
        <fieldset id="shapeSelect">
          <label for="shape">Shape:</label>
          <select id="shape" name="shape" onchange="insertShapeFields()" >
            <option value="blank">&lt;Please select a shape&gt;</option>          
            <option value="circle">Circle</option>
            <option value="square">Square</option>
            <option value="rectangle">Rectangle</option>
          </select>
        </fieldset>

        <fieldset>
          <label for="x_coordinate">X Coordinate:</label>
          <input id="x_coordinate" name="x_coordinate" type="number" step="5" 
              placeholder="-300 to 300" />
        </fieldset>

        <fieldset>
          <label for="y_coordinate">Y Coordinate:</label>
          <input id="y_coordinate" name="y_coordinate" type="number" step="5"
              placeholder="-200 to 200" />
        </fieldset>

        <fieldset id="otherFields">  
        </fieldset>

        <fieldset>
          <label for="colour">Colour:</label>
          <input type="color" id="colour" name="colour" />
        </fieldset>

        <fieldset>
          <input type="submit" />
          <input type="submit" value="Clear" name="clear" />
        </fieldset>
      </form>
      <?php 
        if ($err) {
          echo $err;
        } 
      ?>
      <div>
        <h2>Some Shapes</h2>
        <canvas id="myCanvas" width="800" height="500" style="border: 1px solid black"></canvas>
        <?php 
          echo "<script>let canvas = document.getElementById('myCanvas'); 
              let context = canvas.getContext('2d'); context.translate(400, 250);  
              $drawing</script>";
        ?>
      </div>      
    </main>

    <?php require("footer.php"); ?>

    <script>
      function insertShapeFields() {
        let shape = document.getElementById('shape').value;
        if (shape == 'circle') {
          document.getElementById('otherFields').innerHTML = 
              '<label for="radius">Radius:</label>\n<input type="number" id="radius" name="radius" step="5" placeholder="5 to 200" />';
        } else if (shape == 'rectangle') {
          document.getElementById('otherFields').innerHTML = 
              '<label for="width">Width:</label>\n<input type="number" id="width" name="width" step="5" placeholder="5 to 600" /><br />'
            + '<label for="height">Height:</label>\n<input type="number" id="height" name="height" step="5" placeholder="5 to 400" />';
        } else if (shape == 'square') {
          document.getElementById('otherFields').innerHTML = 
              '<label for="length">Length:</label>\n<input type="number" id="length" name="length" step="5" placeholder="5 to 400" />';
        }
      }
    </script>
  </body>
</html>


