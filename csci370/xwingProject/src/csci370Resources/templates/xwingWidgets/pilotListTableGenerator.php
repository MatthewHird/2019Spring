<?php // author: Matthew Hird
// csciId: hirdm
// date: April 13, 2019
$stylesheets[] = 'xwingIcons.css';

class PilotListTableGenerator {
    
    const CLS_WIDGET = 'plt';
    const CLS_TABLE = self::CLS_WIDGET.'-table';
    const CLS_HEAD_ROW = self::CLS_WIDGET.'-head-row';
    const CLS_HEAD_CELL = self::CLS_WIDGET.'-head-cell';
    const CLS_ROW = self::CLS_WIDGET.'-row';
    const CLS_CELL = self::CLS_WIDGET.'-cell';
    const CLS_PS_CELL = self::CLS_WIDGET.'-ps-cell';
    const CLS_NAME_CELL = self::CLS_WIDGET.'-na-cell';
    const CLS_ELITE_CELL = self::CLS_WIDGET.'-el-cell';
    const CLS_COST_CELL = self::CLS_WIDGET.'-pc-cell';
    const CLS_UNIQUE_INBL = self::CLS_WIDGET.'-uni-inbl';

    const PILOT_LINK_TAG = "<a href='pilotPage.php?pilotId=%s'>%s</a>";

    const QUERY = array(
        // Order 'unique' pilots by pilot skill DESC, then order non-unique pilots
        '1' => "SELECT id, name, variant, u_name_id, pilot_skill, elite, point_cost FROM pilot WHERE ship_id = ? ORDER BY CASE WHEN u_name_id = 0 THEN 0 ELSE 1 END DESC, pilot_skill DESC, point_cost DESC, name"
    );

    public static function getAsHtmlString($pdoConn, $shipId, $htmlId='') {
        $pilots = self::getPilots($pdoConn, $shipId);

        if (!$pilots[0]) {
            return $pilots;
        }

        return array(true, self::generateHtmlString($htmlId, $pilots[1]));
    }

    private static function getPilots($conn, $shipId) {
        try {
            $stmt1 = $conn->prepare(self::QUERY['1']);
            $stmt1->bindParam(1, $shipId);
            $stmt1->execute();

            if($pilots = $stmt1->fetchAll()) {
                $stmt1 = null;
                return array(true, $pilots);
            } else {
                $stmt1 = null;
                return array(false, "Error: no pilots found for ship ID $shipId");
            }
        } catch(PDOException $e) {
            $stmt1 = null;
            return array(false, "Error: ".$e->getMessage());
        }
    }

    private static function generateHtmlString($htmlId, $pilotArray) {
        $htmlString = sprintf("<div id='%s' class='%s'><table class='%s'>", $htmlId, self::CLS_WIDGET, self::CLS_TABLE);
        $htmlString .= sprintf("<tr class='%s'><th class='%s %s'>PS</th><th class='%s %s'>Pilot Name</th><th class='%s %s'>Elite?</th><th class='%s %s'>Point Cost</th></tr>", 
                self::CLS_HEAD_ROW, self::CLS_HEAD_CELL, self::CLS_PS_CELL, self::CLS_HEAD_CELL, self::CLS_NAME_CELL, 
                self::CLS_HEAD_CELL, self::CLS_ELITE_CELL, self::CLS_HEAD_CELL, self::CLS_COST_CELL);
        
        foreach ($pilotArray as $key => $pilot) {
            $ps = ($pilot['pilot_skill'] == -1) ? '*' : $pilot['pilot_skill'];
            $unique = ($pilot['u_name_id'] == 0) ? '' : '&lowast;';
            $pilotName = sprintf(self::PILOT_LINK_TAG, $pilot['id'], $pilot['variant'] ? $pilot['name'].' ('.$pilot['variant'].')' : $pilot['name']);
            $elite = $pilot['elite'] ? 'Yes' : 'No';
            $cost = is_null($pilot['point_cost']) ? 'N/A' : $pilot['point_cost'];
            $htmlString .= sprintf("<tr class='%s'><td class='%s %s'>%s</td><td class='%s %s'><div class='%s'>%s</div>%s</td><td class='%s %s'>%s</td><td class='%s %s'>%s</td></tr>", self::CLS_ROW, self::CLS_CELL, self::CLS_PS_CELL, $ps, self::CLS_CELL, self::CLS_NAME_CELL, self::CLS_UNIQUE_INBL, $unique, $pilotName, self::CLS_CELL, self::CLS_ELITE_CELL, $elite, self::CLS_CELL, self::CLS_COST_CELL, $cost);
        }

        $htmlString .= "</table></div>";
        return $htmlString;
    }
}

?>
