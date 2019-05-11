<?php
	$animal1;
	$animal2;
	$hp;
	$dex;
	$col;
	$err;
	if(isset($_POST)){
		$animal1=$_POST['animal1'];
		$animal2=$_POST['animal2'];
		$hp=$_POST['hitPoints'];
		$dex=$_POST['dexterity'];
		$col=$_POST['colour'];
		if(trim($animal1)==='' || trim($animal2)==='' || trim($hp)==='' || 	
			trim($dex)==='' || trim($col)===''){
			$err = "Sorry, all values must be filled in";	
		}
	}
?>
<!DOCTYPE html>
<!-- put your comments here-->
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
	<meta charset="utf-8"/>
	<title>Result</title>
	<link rel="stylesheet" type="text/css" href="sharedStyles.css">
	<!-- other head stuff -->
</head>
<body >
	<?php
		if(isset($err)){
			echo "<p>".$err."</p>\n";
			echo "<a href='Lab3.html'>Back</a>\n";
		}else{
	?>
	<h1>You Created the Following Beast:</h1>
	<p>Name: <?php echo $col." ".$animal1." ".$animal2;?></p>
	<p>Hit Points: <?php echo $hp;?></p>
	<p>Dexterity: <?php echo $dex;?></p>
	<?php
		}
	?>

</body>
</html>
