<?php 
// author: Matthew Hird
// csciId: hirdm
// date: April 13, 2019
$stylesheets[] = 'xwingIcons.css';

class PilotCardTableGenerator {
    
    const CLS_WIDGET = 'pct';
    const CLS_TABLE = self::CLS_WIDGET.'-table';

    const CLS_ROW = self::CLS_WIDGET.'-row';
    const CLS_CELL = self::CLS_WIDGET.'-cell';
    const CLS_NAME_CELL = self::CLS_WIDGET.'-name-cell';
    const CLS_VAL_CELL = self::CLS_WIDGET.'-value-cell';

    const CLS_UNIQUE_CELL = self::CLS_WIDGET.'-un-cell';
    const CLS_SHIP_CELL = self::CLS_WIDGET.'-sh-cell';
    const CLS_FACTION_CELL = self::CLS_WIDGET.'-fa-cell';
    const CLS_PS_CELL = self::CLS_WIDGET.'-ps-cell';
    const CLS_ELITE_CELL = self::CLS_WIDGET.'-el-cell';
    const CLS_COST_CELL = self::CLS_WIDGET.'-pc-cell';

    const UNAME_LINK_TAG = "<a href='searchPage.php?resultType=pilots&pilotName=%s'>%s</a>";
    const SHIP_LINK_TAG = "<a href='shipPage.php?shipId=%s'>%s</a>";
    const FACTION_LINK_TAG = "<a href='searchPage.php?resultType=ships&faction=%s'>%s</a>";
    const ROW_TAG = "<tr class='%s'><th class='%s %s'>%s</th><td class='%s %s %s'>%s</td></tr>";

    const QUERY = array(
        // Order 'unique' pilotCards by pilot skill DESC, then order non-unique pilotCards
        '1' => "SELECT P.name AS p_name, P.variant AS p_variant, UN.name AS u_name, UN.id AS u_id, P.pilot_skill, P.elite, P.point_cost, S.name AS s_name, S.variant AS s_variant, S.faction, S.id AS s_id FROM pilot P JOIN ship S ON P.ship_id = S.id JOIN unique_name UN ON P.u_name_id = UN.id WHERE P.id = ?"
    );

    public static function getAsHtmlString($pdoConn, $pilotId, $htmlId='') {
        $pilotCards = self::getPilots($pdoConn, $pilotId);

        if (!$pilotCards[0]) {
            return $pilotCards;
        }

        return array(true, self::generateHtmlString($htmlId, $pilotCards[1][0]));
    }

    private static function getPilots($conn, $pilotId) {
        try {
            $stmt1 = $conn->prepare(self::QUERY['1']);
            $stmt1->bindParam(1, $pilotId);
            $stmt1->execute();

            if($pilotCards = $stmt1->fetchAll()) {
                $stmt1 = null;
                return array(true, $pilotCards);
            } else {
                $stmt1 = null;
                return array(false, "Error: no pilotCards found for ship ID $shipId");
            }
        } catch(PDOException $e) {
            $stmt1 = null;
            return array(false, "Error: ".$e->getMessage());
        }
    }

    private static function generateHtmlString($htmlId, $pilotCardArray) {
        $pilotNameFull = $pilotCardArray['p_variant'] ? $pilotCardArray['p_name'].' ('.$pilotCardArray['p_variant'].')' : $pilotCardArray['p_name'];

        $uniqueName = ($pilotCardArray['u_id'] > 0 ? sprintf(self::UNAME_LINK_TAG, $pilotCardArray['u_name'], $pilotCardArray['u_name']) : 'N/A');

        $shipName = sprintf(self::SHIP_LINK_TAG, $pilotCardArray['s_id'], ($pilotCardArray['s_variant'] ? $pilotCardArray['s_name'].' ('.$pilotCardArray['s_variant'].')' : $pilotCardArray['s_name']));
        
        $faction = sprintf(self::FACTION_LINK_TAG, $pilotCardArray['faction'], $pilotCardArray['faction']);


        $uniqueNameRow = sprintf(self::ROW_TAG, self::CLS_ROW, self::CLS_CELL, self::CLS_NAME_CELL, 'Unique Name', self::CLS_CELL, self::CLS_VAL_CELL, self::CLS_UNIQUE_CELL, $uniqueName);

        $shipNameRow = sprintf(self::ROW_TAG, self::CLS_ROW, self::CLS_CELL, self::CLS_NAME_CELL, 'Ship Name', self::CLS_CELL, self::CLS_VAL_CELL, self::CLS_SHIP_CELL, $shipName);

        $factionRow = sprintf(self::ROW_TAG, self::CLS_ROW, self::CLS_CELL, self::CLS_NAME_CELL, 'Faction', self::CLS_CELL, self::CLS_VAL_CELL, self::CLS_FACTION_CELL, $faction);

        $pilotSkillRow = sprintf(self::ROW_TAG, self::CLS_ROW, self::CLS_CELL, self::CLS_NAME_CELL, 'Pilot Skill', self::CLS_CELL, self::CLS_VAL_CELL, self::CLS_PS_CELL, (isset($pilotCardArray['pilot_skill']) ? $pilotCardArray['pilot_skill'] : '&lowast;'));

        $eliteRow = sprintf(self::ROW_TAG, self::CLS_ROW, self::CLS_CELL, self::CLS_NAME_CELL, 'Elite?', self::CLS_CELL, self::CLS_VAL_CELL, self::CLS_ELITE_CELL, ($pilotCardArray['elite'] ? 'Yes' : 'No'));

        $pointCostRow = sprintf(self::ROW_TAG, self::CLS_ROW, self::CLS_CELL, self::CLS_NAME_CELL, 'Point Cost', self::CLS_CELL, self::CLS_VAL_CELL, self::CLS_COST_CELL, (isset($pilotCardArray['point_cost']) ? $pilotCardArray['point_cost'] : 'N/A'));

        $htmlString = sprintf("<div id='%s' class='%s'>$s<table class='%s'>%s%s%s%s%s%s</table></div>", 
                $htmlId, self::CLS_WIDGET, 
                self::CLS_TABLE, $uniqueNameRow, $shipNameRow, $factionRow, $pilotSkillRow, $eliteRow, $pointCostRow);

        return $htmlString;
    }
}

?>
