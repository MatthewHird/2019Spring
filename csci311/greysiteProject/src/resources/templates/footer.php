<?php
// author: Matthew Hird
// csciId: hirdm
// date: Mar 6, 2019
// links: 
echo "</main>
<footer class='footer'>
  <small>Copyright &copy; $author_name, " . date('Y') . "</small>
</footer>\n";

if (isset($jsImports)) {
  $jsImports = array_unique ($jsImports);
  foreach ($jsImports as $key => $value) {
    echo "<script src='js/$value'></script>\n";
  }
}
echo "</body>
</html>";
?>