<!-- 
  author: Matthew Hird
  csciId: hirdm
  date: Mar 6, 2019
  links: 
-->
<?php
  require_once("requireLogin.php");

  $author_name = "Matthew Hird";
  $page_title = "Lab 8 Loops - Matt's CSCI 311";
  $h1_header = "Lab 8 Loops";

  require("head.php");
?>

<?php
  $rows;
  $cols;
  $err = "";

  if ( ! empty($_REQUEST) ) {  
    $rows = (int) $_REQUEST['rowCount'];
    $cols = (int) $_REQUEST['columnCount'];
    if ($rows < 1 || $rows > 10) {
      $err .= "Row count must be between 1 and 10 <br />";
    }
    if ($cols < 1 || $cols > 6) {
      $err .= "Column count must be between 1 and 6 <br />";
    }
  }
?>
  <body>

    <?php require("header.php"); ?>

    <main>
      <form action="Lab8Loops.php" method="POST">
        <fieldset>
          <label for="rowCount">Table Rows</label>
          <input type="number" id="rowCount" name="rowCount" />
        </fieldset>
          
        <fieldset>
          <label for="columnCount">Table Columns</label>
          <input type="number" id="columnCount" name="columnCount" />
        </fieldset>

        <fieldset>
          <input type="submit" value="Generate Table" />
        </fieldset>
      </form>
      <?php 
        if ($err) {
           echo $err;
        } else {
            echo "<div class='myTable'>";
          for ($i=0; $i < $rows; $i++) {
            $oddRow = ($i % 2 == 1) ? "oddRow" : "";
            echo "<div class='myRow $oddRow'>";

            for ($j=0; $j < $cols; $j++) { 
              echo "<span class='myCell'>Rows: $rows Columns: $cols</span>";
            }
            echo "</div>";
          }
          echo "</div>";
        } 
      ?>     
    </main>

    <?php require("footer.php"); ?>

  </body>
</html>