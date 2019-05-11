<!-- 
// author: Matthew Hird
// csciId: hirdm
// date: April 13, 2019
-->
<?php

require_once('../csci370Resources/config.php');

require_once(WIDGETS.'pilotCardTableGenerator.php');

$author_name = "Matthew Hird";

$stylesheets[] = 'xwingProject.css';


function handleGetRequest($get) {
    $query = array(
        '1' => "SELECT name, variant FROM pilot WHERE id = ?"
    );
    if (isset($get['pilotId'])) {
        try {
            $pilotId = htmlspecialchars(trim($get['pilotId']));
            $conn = dbconnect();

            $stmt1 = $conn->prepare($query['1']);
            
            $stmt1->bindParam(1, $pilotId);
            $stmt1->execute();
            $stmt1->bindColumn('name', $pilotName);
            $stmt1->bindColumn('variant', $pilotVariant);

            if (!$stmt1->fetch(PDO::FETCH_BOUND)) {
                $stmt1 = null;
                $conn = null;
                return false;
            }
            $stmt1 = null;

            $title = $pilotVariant ? $pilotName.' ('.$pilotVariant.')' : $pilotName ;
            $htmlString = 
                "<div class='standard-layout'>"
                    ."<h1>$title</h1>"
                    ."<hr>" 
                    ."<div>"
                        ."<div class='display-container-ib'>"
                            ."<div class='pilot-card-div'>"
                                .PilotCardTableGenerator::getAsHtmlString($conn, $pilotId, 'shipInfo1')[1]
                            ."</div>"    
                        ."</div>"    
                    ."</div>"
                ."</div>";
        } catch(PDOException $e) {
            $stmt1 = null;
            return array(false, "Error: ".$e->getMessage());
        }

        $conn = null;
        return array($title, $htmlString);
    }
}


if ($getResult = handleGetRequest($_GET)) {
    $page_title = $getResult[0]." - Pilot Page - $siteTitle";
    // $h1_header = $getResult[0];
    $pilotDisplay = $getResult[1];
} else {
    header("Location: pilotPage.php?pilotId=5"); 
    exit();
    header("Location: home.php"); 
}


// All stylesheets must be added to $stylesheets[] before importing 'head.php'
require_once(GEN_TEMPLATES."head.php");
require_once(GEN_TEMPLATES."header.php");

// echo "<img class='tn-siteLogo' src='$siteIcon75x75' alt='$siteTitle logo' >";
if (isset($pilotDisplay)) {
    echo $pilotDisplay;
}




require_once(GEN_TEMPLATES."footer.php");

?>
