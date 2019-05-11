<?php 
// author: Matthew Hird
// csciId: hirdm
// date: April 13, 2019
$stylesheets[] = 'xwingIcons.css';

class ShipInfoHeaderGenerator {
    
    const CLS_WIDGET = 'sih';
    const CLS_TABLE = self::CLS_WIDGET.'-table';
    const CLS_ROW = self::CLS_WIDGET.'-row';
    const CLS_CELL = self::CLS_WIDGET.'-cell';
    const CLS_HEAD_CELL = self::CLS_WIDGET.'-head-cell';
    const CLS_VAL_CELL = self::CLS_WIDGET.'-value-cell';
    
    const ROW_TAG = "<tr class='%s'><th class='%s %s'>%s</th><td class='%s %s'>%s</td></tr>";
    const FACTION_LINK_TAG = "<a href='searchPage.php?resultType=ships&faction=%s'>%s</a>";

    const QUERY = array(
        '1' => 'SELECT name, variant, faction, size FROM ship WHERE id = ?',
        '2' => 'SELECT F.name FROM firing_arc F JOIN ship_firing_arc S ON F.id = S.firing_arc_id WHERE S.ship_id = ?',
    );

    public static function getAsHtmlString($pdoConn, $shipId, $htmlId='') {
        $shipInfo = self::getShipInfo($pdoConn, $shipId);
        if (!$shipInfo[0]) {
            return $shipInfo;
        }

        return array(true, self::generateHtmlString($htmlId, $shipInfo[1][0], $shipInfo[2]));
    }

    private static function getShipInfo($conn, $shipId) {
        try {

            $stmt1 = $conn->prepare(self::QUERY['1']);
            
            $stmt1->bindParam(1, $shipId);
            $stmt1->execute();

            if (!$shipInfo = $stmt1->fetchAll()) {
                $stmt1 = null;
                return array(false, "Error: ship with ID $shipId not in system");
            }
            $stmt1 = null;
            
            $stmt2 = $conn->prepare(self::QUERY['2']);
            $stmt2->bindParam(1, $shipId);
            $stmt2->execute();

            // Ship can have 0, 1 or many firing arcs
            $firingArcs = $stmt2->fetchAll(PDO::FETCH_NUM);
            $stmt2=null;
            return array(true, $shipInfo, $firingArcs);

        } catch(PDOException $e) {
            $stmt1 = null;
            $stmt2 = null;
            return array(false, "Error: ".$e->getMessage());
        }
    }

    private static function generateHtmlString($htmlId, $shipArray, $firingArcArray) {
        $name = sprintf(self::ROW_TAG, self::CLS_ROW, self::CLS_CELL, self::CLS_HEAD_CELL, 'Ship Name', 
                self::CLS_CELL, self::CLS_VAL_CELL, 
                ($shipArray['variant'] ? $shipArray['name'].' ('.$shipArray['variant'].')' : $shipArray['name']));
        
        $faction = sprintf(self::FACTION_LINK_TAG, $shipArray['faction'], $shipArray['faction']);

        $factionRow = sprintf(self::ROW_TAG, self::CLS_ROW, self::CLS_CELL, self::CLS_HEAD_CELL, 'Faction', 
                self::CLS_CELL, self::CLS_VAL_CELL, $faction);
        $size = sprintf(self::ROW_TAG, self::CLS_ROW, self::CLS_CELL, self::CLS_HEAD_CELL, 'Size', 
                self::CLS_CELL, self::CLS_VAL_CELL, $shipArray['size']);

        $firingArcCell = 'None';
        if (gettype($shipArray) == 'array') {
            $firingArcCell = $firingArcArray[0][0];
            for ($i=1; $i < sizeof($firingArcArray); $i++) { 
                $firingArcCell .= '<br>'.$firingArcArray[$i][0];
            }
        }
        $firingArcRow = sprintf(self::ROW_TAG, self::CLS_ROW, self::CLS_CELL, self::CLS_HEAD_CELL, 'Firing Arcs', 
                self::CLS_CELL, self::CLS_VAL_CELL, $firingArcCell);


        $htmlString = sprintf("<div id='%s' class='%s'><table class='%s'>%s%s%s%s</table></div>", 
                $htmlId, self::CLS_WIDGET, self::CLS_TABLE, $name, $factionRow, $size, $firingArcRow);

        return $htmlString;
    }
}

?>
