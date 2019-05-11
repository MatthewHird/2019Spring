<?php 
// author: Matthew Hird
// csciId: hirdm
// date: April 13, 2019
$stylesheets[] = 'xwingIcons.css';

class UpgradeBarGenerator {
    
    const CLS_WIDGET = 'upb';
    const CLS_CELL = self::CLS_WIDGET.'-cell';

    const ICON_TAG = '<div class="%s"></div>';
    const CLS_BLANK = 'blank-30';

    const UPG_REF = array('Title' => 'tit', 'Modification' => 'mod', 'Elite' => 'eli', 'System' => 'sys', 'Cannon' => 'can', 
            'Turret' => 'tur', 'Torpedo' => 'tor', 'Missile' => 'mis', 'Bomb' => 'bom', 'Crew' => 'cre', 'Astromech' => 'ast', 
            'Salvaged Astromech' => 'sal', 'Tech' => 'tec', 'Illicit' => 'ill', 'Hardpoint' => 'har', 'Team' => 'tea', 'Cargo' => 'car');

    const QUERY = array(
        '1' => 'SELECT U.name  FROM upgrade_bar UB JOIN upgrade U ON upgrade_id = U.id WHERE ship_id = ? ORDER BY position DESC'
    );

    public static function getAsHtmlString($pdoConn, $shipId, $elite=false, $colour='-b', $htmlId='') {
        if ($colour != '-b' && $colour != '-w') {
            return array(false, "Error: colour must be '-b' for black or '-w' for white");
        }

        $upgrades = self::getUpgrades($pdoConn, $shipId);

        if (!$upgrades[0]) {
            return $upgrades;
        }

        $upgradeArray = array();
        if (gettype($upgrades[1] == 'array')) {
            foreach ($upgrades[1] as $key => $value) {

                $upgradeArray[] = $value[0];
            }
        }
        if ($elite) {
            $upgradeArray[] = 'Elite';
        }

        return array(true, self::generateHtmlString($htmlId, $colour, $upgradeArray));
    }

    private static function getUpgrades($conn, $shipId) {
        try {
            $stmt1 = $conn->prepare(self::QUERY['1']);            
            $stmt1->bindParam(1, $shipId);
            $stmt1->execute();

            // # of upgrade icons can be 0
            $upgrades = $stmt1->fetchAll(PDO::FETCH_NUM);
            $stmt1 = null;
            return array(true, $upgrades);
            
        } catch(PDOException $e){
            $stmt1 = null;
            return array(false, "Error: ".$e->getMessage());
        }
    }

    private static function generateHtmlString($htmlId, $colour, $upgradeArray) {
        $htmlString = "</div>";
        for ($i=0; $i < 7; $i++) { 
            if (isset($upgradeArray[$i])) {
                $iconTag = sprintf(self::ICON_TAG, self::UPG_REF[$upgradeArray[$i]].$colour);
            } else {
                $iconTag = sprintf(self::ICON_TAG, self::CLS_BLANK);
            }
            $htmlString = sprintf("<div class='%s'>%s</div>", self::CLS_CELL, $iconTag).$htmlString;
        }
        $htmlString = sprintf("<div id='%s' class='%s'>", $htmlId, self::CLS_WIDGET).$htmlString;
        return $htmlString;
    }

}

?>
