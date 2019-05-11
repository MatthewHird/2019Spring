<?php 
// author: Matthew Hird
// csciId: hirdm
// date: April 13, 2019
$stylesheets[] = 'xwingIcons.css';

class ManeuverDialTableGenerator {
    private $manTableArray;

    const CLS_WIDGET = 'mdt';
    const CLS_ROW = self::CLS_WIDGET.'-row';
    const CLS_CELL = self::CLS_WIDGET.'-cell';

    const ICON_TAG = '<div class="%s"></div>';
    const CLS_BLANK = 'blank-30';
    const CLS_SPEED = 'sp-';

    const MAN_REF = array(array('Turn Left', 't-l', 1), array('Bank Left', 'b-l', 2), array('Straight', 'sm', 3), 
            array('Bank Right', 'b-r',4), array('Turn Right', 't-r', 5), array('Koiogran Turn', 'kt', 7), 
            array('Segnors Loop Left', 'sl-l', 6), array('Segnors Loop Right', 'sl-r', 8), array('Tallon Roll Left', 'tr-l', 6), 
            array('Tallon Roll Right', 'tr-r', 8), array('Stationary', 'sta', 3), array('Reverse Straight', 'rsm', 7), 
            array('Reverse Bank Left', 'rb-l', 6), array('Reverse Bank Right', 'rb-r', 8), array('Huge Turn Left', 'b-l', 2), 
            array('Huge Straight', 'sm', 3), array('Huge Turn Right', 'b-r', 4));

    const QUERY = array(
        '1' => 'SELECT size FROM ship WHERE id = ?',
        '2' => 'SELECT MD.speed, M.name, MD.difficulty FROM maneuver_dial MD JOIN maneuver M ON maneuver_id = M.id WHERE ship_id = ?',
        '3' => 'SELECT HD.speed, M.name, HD.energy FROM huge_maneuver_dial HD JOIN maneuver M ON maneuver_id = M.id WHERE ship_id = ?'
    );

    public static function getAsHtmlString($pdoConn, $shipId, $htmlId='') {
        $maneuvers = self::getManeuvers($pdoConn, $shipId);

        if (!$maneuvers[0]) {
            return $maneuvers;
        }
        
        $manTableArray = self::initializeManTableArray();
        self::fillManTableArray($manTableArray, $maneuvers[1], $maneuvers[2]);
        return array(true, self::generateHtmlString($htmlId, $manTableArray));
    }

    private static function getManeuvers($conn, $shipId) {
        try {
            $stmt1 = $conn->prepare(self::QUERY['1']);
            
            $stmt1->bindParam(1, $shipId);
            $stmt1->execute();
            $stmt1->bindColumn('size', $shipSize);

            if (!$stmt1->fetch(PDO::FETCH_BOUND)) {
                $stmt1 = null;
                return array(false, "Error: ship with ID $shipId not in system");
            }
            $stmt1 = null;

            if ($shipSize == 'Huge') {
                $stmt2 = $conn->prepare(self::QUERY['3']);
            } else {
                $stmt2 = $conn->prepare(self::QUERY['2']);
            }
            $stmt2->bindParam(1, $shipId);
            $stmt2->execute();

            if($maneuvers = $stmt2->fetchAll()) {
                $stmt2 = null;
                return array(true, $maneuvers, $shipSize);
            } else {
                $stmt2 = null;
                return array(false, "Error: no maneuvers found for ship ID $shipId");
            }
        } catch(PDOException $e) {
            $stmt1 = null;
            $stmt2 = null;
            return array(false, "Error: ".$e->getMessage());
        }
    }

    private static function initializeManTableArray() {
        $manTableArray = array();
        for ($i=0; $i < 6; $i++) { 
            $manTableArray[$i] =  array();
            $manTableArray[$i][0] = array(true, sprintf(self::ICON_TAG, self::CLS_SPEED.(5 - $i)));
            for ($j=1; $j < 9; $j++) { 
                $manTableArray[$i][$j] = array(false, sprintf(self::ICON_TAG, self::CLS_BLANK));
            }
        }
        return $manTableArray;
    }

    private static function fillManTableArray(&$manTableArray, $maneuverArray, $shipSize) {
        foreach ($maneuverArray as $key => $maneuver) {
            $row = 5 - $maneuver['speed'];
            $col = -1;
            $name = $maneuver['name'];

            if ($shipSize == 'Huge') {
                // $mod = strtolower($maneuver['energy']);
                $mod = '-w';
            } else {
                $mod = '-'.strtolower($maneuver['difficulty']);
            }
            $cls = '';

            foreach (self::MAN_REF as $key => $refTuple) {
                if ($name == $refTuple[0]) {
                    $cls = $refTuple[1].$mod;
                    $col = $refTuple[2];
                    break;
                }
            }
            
            if ($col > 0 && $col < 9) {
                $manTableArray[$row][$col][0] = true;
                $manTableArray[$row][$col][1] = sprintf(self::ICON_TAG, $cls);
            }
        }
    }

    private static function generateHtmlString($htmlId, $manTableArray) {
        $htmlString = sprintf("<div id='%s' class='%s'>", $htmlId, self::CLS_WIDGET);
        $emptyRows = self::checkEmptyRows($manTableArray);
        $emptyCols = self::checkEmptyCols($manTableArray);

        for ($i=0; $i < 6; $i++) { 
            if (!$emptyRows[$i]) {
                $htmlString .= sprintf("<div class='%s'>", self::CLS_ROW);
                
                for ($j=0; $j < 9; $j++) {
                    if (!$emptyCols[$j]) {
                        $htmlString .= sprintf("<div class='%s'>%s</div>", self::CLS_CELL, $manTableArray[$i][$j][1]);
                     } 
                }

                $htmlString .= "</div>";
            }
        }
        $htmlString .= "</div>";
        return $htmlString;
    }

    private static function checkEmptyRows($manTableArray) {
        $emptyRows = array();
        for ($i=0; $i < 6; $i++) { 
            $empty = true;
            for ($j=1; $j < 9; $j++) { 
                if ($manTableArray[$i][$j][0] == true) {
                    $empty = false;
                }
            }
            $emptyRows[$i] = $empty;
        }
        return $emptyRows;
    }

    private static function checkEmptyCols($manTableArray) {
        $emptyCols = array();
        for ($j=1; $j < 9; $j++) { 
            $empty = true;
            for ($i=0; $i < 6; $i++) { 
                if ($manTableArray[$i][$j][0] == true) {
                    $empty = false;
                }
            }
            $emptyCols[$j] = $empty;
        }
        return $emptyCols;
    }
}

?>
