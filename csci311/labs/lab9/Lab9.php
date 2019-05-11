<!-- 
author: Matthew Hird
csciId: hirdm
date: Mar 16, 2019
links: 
-->

<?php
  require("dbinfo.inc");
  
  $page_title = $user;

  require("front.php");
?>
  <header>
    <h1>"Using PHP to access MySQL database"</h1>
  </header>

  <main>

    <?php 
      try {
        $dbh = new PDO("mysql:host=$host;dbname=$database", $user, $password);

        $result = $dbh->query('SELECT * from Automobiles');

        echo 
          "<table>
            <tr>
              <th>Vehicle Name</th>
              <th>Model</th>
              <th>Make</th>
              <th>Description</th>
              <th>Price</th>
            </tr>";

        foreach($result as $row) {
          $nameVal = $row['vehicle_name'];
          $modelVal = $row['model'];
          $makeVal = $row['make'];
          $desc = $row['description'];
          $price = $row['price'];

          echo 
            "<tr>
              <td>$nameVal</td>
              <td>$modelVal</td>
              <td>$makeVal</td>
              <td>$desc</td>
              <td>\$$price</td>
            </tr>";
        }
        echo "</table>";
      }catch(PDOException $e){
        print "Error!" . $e->getMessage() . "<br/>";
      }
    ?>

  </main>
  
  <?php require("back.php"); ?>