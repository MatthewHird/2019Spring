<!-- 
// author: Matthew Hird
// csciId: hirdm
// date: April 13, 2019
-->
<?php

require_once('../csci370Resources/config.php');

require_once(WIDGETS.'maneuverDialTableGenerator.php');
require_once(WIDGETS.'upgradeBarGenerator.php');
require_once(WIDGETS.'actionBarGenerator.php');
require_once(WIDGETS.'pilotListTableGenerator.php');
require_once(WIDGETS.'shipStatsTableGenerator.php');
require_once(WIDGETS.'shipInfoHeaderGenerator.php');

$author_name = "Matthew Hird";

$stylesheets[] = 'xwingProject.css';


function handleGetRequest($get) {
    $query = array(
        '1' => "SELECT name, variant FROM ship WHERE id = ?"
    );
    if (isset($get['shipId'])) {
        try {
            $shipId = htmlspecialchars(trim($get['shipId']));
            $conn = dbconnect();

            $stmt1 = $conn->prepare($query['1']);
            
            $stmt1->bindParam(1, $shipId);
            $stmt1->execute();
            $stmt1->bindColumn('name', $shipName);
            $stmt1->bindColumn('variant', $shipVariant);

            if (!$stmt1->fetch(PDO::FETCH_BOUND)) {
                $stmt1 = null;
                $conn = null;
                return false;
            }
            $stmt1 = null;

            $title = $shipVariant ? $shipName.' ('.$shipVariant.')' : $shipName ;
            $htmlString = 
                "<div class='standard-layout'>"
                    ."<h1>$title</h1>"
                    ."<hr>"
                    ."<div>"
                        ."<div class='display-container-ib'>"
                            ."<div class='ship-info-div'>"
                                .ShipInfoHeaderGenerator::getAsHtmlString($conn, $shipId, 'shipInfo1')[1]
                            ."</div>"
                        ."</div>"
                        ."<div class='display-container-ib'>"
                            ."<div class='ship-stats-div'>"
                                .ShipStatsTableGenerator::getAsHtmlString($conn, $shipId, 'shipStats1')[1]
                            ."</div>"
                        ."</div>"
                    ."</div>"
                    ."<div>"
                        ."<div class='display-container-ib'>"
                            ."<div class='display-container'>"
                                ."<div class='action-div'>"
                                    ."<div class='action-header'>Action Bar</div>"
                                    .ActionBarGenerator::getAsHtmlString($conn, $shipId, 'actBar1')[1]
                                ."</div>"
                            ."</div>"
                            ."<div class='display-container'>"
                                ."<div class='upgrade-div'>"
                                    ."<div class='upgrade-header'>Upgrade Bar</div>"
                                    .UpgradeBarGenerator::getAsHtmlString($conn, $shipId, false, '-w', 'upBar1')[1]
                                ."</div>"
                            ."</div>"
                        ."</div>"
                        ."<div class='display-container-ib'>"
                            ."<div class='maneuver-div'>"
                                ."<div class='maneuver-header-outer'><div class='maneuver-header-inner'>Maneuver Dial</div></div>"
                                .ManeuverDialTableGenerator::getAsHtmlString($conn, $shipId, 'manDial1')[1]
                            ."</div>"
                        ."</div>"
                    ."</div>" 
                    ."<div>"
                        ."<div class='display-container-ib'>"
                            ."<div class='pilot-list-div'>"
                                .PilotListTableGenerator::getAsHtmlString($conn, $shipId, 'pilList1')[1]
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
    $page_title = $getResult[0]." - Ship Page - $siteTitle";
    // $h1_header = $getResult[0];
    $shipDisplay = $getResult[1];
} else {
    header("Location: shipPage.php?shipId=45"); 
    exit();
    header("Location: home.php"); 
}


// All stylesheets must be added to $stylesheets[] before importing 'head.php'
require_once(GEN_TEMPLATES."head.php");
require_once(GEN_TEMPLATES."header.php");

// echo "<img class='tn-siteLogo' src='$siteIcon75x75' alt='$siteTitle logo' >";
if (isset($shipDisplay)) {
    echo $shipDisplay;
}




require_once(GEN_TEMPLATES."footer.php");

?>




