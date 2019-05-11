<!-- 
  author: Matthew Hird
  csciId: hirdm
  date: Mar 6, 2019
  links: 
-->
<?php
  require_once("requireLogin.php");

  $author_name = "Matthew Hird";
  $page_title = "Lab 8 Arrays - Matt's CSCI 311";
  $h1_header = "Lab 8 Arrays";

  require("head.php");
?>

<?php
  $err = "";
  $myAnimal = "";

  var_dump($_POST);
  die("haaaiii");
  if (!empty($_POST)) {  
    if ($_POST['animal'] == "blank") {
      $err .= "Animal is a required field <br />";
    }
    if ($_POST['colour'] == "blank") {
      $err .= "Colour is a required field <br />";
    }
    if ($_POST['type'] == "blank") {
      $err .= "Type/Alignment is a required field <br />";
    }
    if (trim($_POST['age']) > 999 || trim($_POST['age']) < 1) {
      $err .= "Age must be between 1 and 999 <br />";
    }
    if (!$err) {
      $myAnimal = array('Animal' => $_POST['animal'], 'Colour' => $_POST['colour'],
                        'Type/Alignment' => $_POST['type'], 'Age' => trim($_POST['age']));
    }
  }
?>
  <body>

    <?php require("header.php"); ?>

    <main>

      <form action="Lab8Arrays.php" method="POST">
        <fieldset>
          <label for="animal">Animal:</label>
          <select id="animal" name="animal">
            <option value="blank">&lt;Please select an animal&gt;</option>          
            <option value="cat">Katter!</option>
            <option value="dog">Doge!</option>
            <option value="sasquatch">Squashwatch!</option>
            <option value="eggsalad">Ergsurlerd!</option>
          </select>
        </fieldset>

        <fieldset>
          <label for="colour">Colour:</label>
          <select id="colour" name="colour">
            <option value="blank">&lt;Please select a colour&gt;</option>          
            <option value="red">Redish Brown</option>
            <option value="green">Greenish Brown</option>
            <option value="brown">Off Brown</option>
            <option value="yellow">Eggsalad Brown</option>
          </select>
        </fieldset>

        <fieldset>
          <label for="type">Type/Alignment:</label>
          <select id="type" name="type">
            <option value="blank">&lt;Please select an alignment&gt;</option>          
            <option value="good">Gurd</option>
            <option value="evil">Bird</option>
            <option value="neutral">Neutron</option>
          </select>
        </fieldset>

        <fieldset>
          <label for="age">Age:</label>
          <input type="number" id="age" name="age" />          
        </fieldset>

        <fieldset>
          <input type="submit" />
        </fieldset>
      </form>
      <?php 
        if ($err) {
          echo $err;
        } else if ($myAnimal) {
          foreach ($myAnimal as $key => $value) {
            echo "<h2>".$key."</h2>
                  <p>".$value."</p>";
          }
        }
      ?>
    </main>

    <?php require("footer.php"); ?>

  </body>
</html>