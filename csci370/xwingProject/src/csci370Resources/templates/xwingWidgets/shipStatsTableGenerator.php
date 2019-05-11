<?php 
// author: Matthew Hird
// csciId: hirdm
// date: April 13, 2019
$stylesheets[] = 'xwingIcons.css';

class ShipStatsTableGenerator {
    
    const CLS_WIDGET = 'sst';
    const CLS_TABLE = self::CLS_WIDGET.'-table';
    const CLS_HEAD_ROW = self::CLS_WIDGET.'-head-row';
    const CLS_HEAD_CELL = self::CLS_WIDGET.'-head-cell';
    const CLS_ROW = self::CLS_WIDGET.'-row';
    const CLS_CELL = self::CLS_WIDGET.'-cell';
    const CLS_NAME_CELL = self::CLS_WIDGET.'-name-cell';
    const CLS_VAL_CELL = self::CLS_WIDGET.'-value-cell';
    const CLS_AT_ROW = self::CLS_WIDGET.'-at-row';
    const CLS_EN_ROW = self::CLS_WIDGET.'-en-row';
    const CLS_AG_ROW = self::CLS_WIDGET.'-ag-row';
    const CLS_HU_ROW = self::CLS_WIDGET.'-hu-row';
    const CLS_SH_ROW = self::CLS_WIDGET.'-sh-row';
    const ROW_TAG = "<tr class='%s %s'><td class='%s %s'>%s Value</td><td class='%s %s'>%s</td></tr>";

    const QUERY = array(
        '1' => 'SELECT size FROM ship WHERE id = ?',
        '2' => 'SELECT attack, agility, hull, shield FROM ship WHERE id = ?',
        '3' => 'SELECT S.attack, HS.energy, S.agility, S.hull, S.shield FROM ship S JOIN huge_ship HS ON S.id = ship_id WHERE S.id = ?'
    );

    public static function getAsHtmlString($pdoConn, $shipId, $htmlId='') {
        $shipStats = self::getShipStats($pdoConn, $shipId);
        if (!$shipStats[0]) {
            return $shipStats;
        }

        return array(true, self::generateHtmlString($htmlId, $shipStats[1][0], $shipStats[2]));
    }

    private static function getShipStats($conn, $shipId) {
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

            if($shipStats = $stmt2->fetchAll()) {
                $stmt2 = null;
                return array(true, $shipStats, $shipSize);
            } else {
                $stmt2 = null;
                return array(false, "Error: no shipStats found for ship ID $shipId");
            }
        } catch(PDOException $e) {
            $stmt1 = null;
            $stmt2 = null;
            return array(false, "Error: ".$e->getMessage());
        }
    }

    private static function generateHtmlString($htmlId, $shipArray, $shipSize) {
        $headerRow = sprintf("<tr class='%s'><th class='%s' colspan='2'>Ship Base Stats</th></tr>", self::CLS_HEAD_ROW, self::CLS_HEAD_CELL);
        $attack = isset($shipArray['attack']) ? sprintf(self::ROW_TAG, self::CLS_ROW, self::CLS_AT_ROW, self::CLS_CELL, 
                self::CLS_NAME_CELL, 'Attack', self::CLS_CELL, self::CLS_VAL_CELL, $shipArray['attack']) : '';
        $energy = isset($shipArray['energy']) ? sprintf(self::ROW_TAG, self::CLS_ROW, self::CLS_EN_ROW, self::CLS_CELL, 
                self::CLS_NAME_CELL, 'Energy', self::CLS_CELL, self::CLS_VAL_CELL, $shipArray['energy']) : '';
        $agility = sprintf(self::ROW_TAG, self::CLS_ROW, self::CLS_AG_ROW, self::CLS_CELL, self::CLS_NAME_CELL, 'Agility', 
                self::CLS_CELL, self::CLS_VAL_CELL, (isset($shipArray['agility']) ? $shipArray['agility'] : 'N/A' ));
        $hull = sprintf(self::ROW_TAG, self::CLS_ROW, self::CLS_HU_ROW, self::CLS_CELL, self::CLS_NAME_CELL, 'Hull', 
                self::CLS_CELL, self::CLS_VAL_CELL, (isset($shipArray['hull']) ? $shipArray['hull'] : 'N/A'));
        $shield = sprintf(self::ROW_TAG, self::CLS_ROW, self::CLS_SH_ROW, self::CLS_CELL, self::CLS_NAME_CELL, 'Shield', 
                self::CLS_CELL, self::CLS_VAL_CELL, (isset($shipArray['shield']) ? $shipArray['shield'] : 'N/A'));

        $htmlString = sprintf("<div id='%s' class='%s'><table class='%s'>%s%s%s%s%s%s</table></div>", $htmlId, self::CLS_WIDGET, self::CLS_TABLE, $headerRow, 
                $attack, $energy, $agility, $hull, $shield);

        return $htmlString;
    }
}

?>
