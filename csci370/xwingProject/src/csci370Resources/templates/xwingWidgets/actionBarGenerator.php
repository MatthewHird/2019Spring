<?php 
// author: Matthew Hird
// csciId: hirdm
// date: April 13, 2019
$stylesheets[] = 'xwingIcons.css';

class ActionBarGenerator {
    
    const CLS_WIDGET = 'acb';
    const CLS_CELL = self::CLS_WIDGET.'-cell';

    const ICON_TAG = '<div class="%s"></div>';
    const CLS_BLANK = 'blank-50';
    const CLS_COLOUR = '-b';

    const UPG_REF = array('Focus' => 'foc', 'Target Lock' => 'tar', 'Barrel Roll' => 'bar', 'Boost' => 'boo', 'Evade' => 'eva', 
            'Cloak' => 'clo', 'Coordinate' => 'coo', 'Jam' => 'jam', 'Recover' => 'rec', 'Reinforce' => 'rei', 'Reload' => 'rel', 
            'Rotate Arc' => 'rot', 'SLAM' => 'sla');

    const QUERY = array(
        '1' => 'SELECT A.name FROM action_bar AB JOIN action A ON action_id = A.id WHERE ship_id = ?'
    );

    public static function getAsHtmlString($pdoConn, $shipId, $htmlId='') {
        $actions = self::getActions($pdoConn, $shipId);

        if (!$actions[0]) {
            return $actions;
        }

        $actionArray = array();
        if (gettype($actions[1] == 'array')) {
            foreach ($actions[1] as $key => $value) {

                $actionArray[] = $value[0];
            }
        }

        return array(true, self::generateHtmlString($htmlId, $actionArray));
    }

    private static function getActions($conn, $shipId) {
        try {
            $stmt1 = $conn->prepare(self::QUERY['1']);
            $stmt1->bindParam(1, $shipId);
            $stmt1->execute();

            // # of action icons can be 0
            $actions = $stmt1->fetchAll(PDO::FETCH_NUM);
            $stmt1 = null;
            return array(true, $actions);
            
        } catch(PDOException $e){
            $stmt1 = null;
            return array(false, "Error: ".$e->getMessage());
        }
    }

    private static function generateHtmlString($htmlId, $actionArray) {
        $htmlString = sprintf("<div id='%s' class='%s'>", $htmlId, self::CLS_WIDGET);
        for ($i=0; $i < 4; $i++) { 
            if (isset($actionArray[$i])) {
                $iconTag = sprintf(self::ICON_TAG, self::UPG_REF[$actionArray[$i]].self::CLS_COLOUR);
            } else {
                $iconTag = sprintf(self::ICON_TAG, self::CLS_BLANK);
            }
            $htmlString .= sprintf("<div class='%s'>%s</div>", self::CLS_CELL, $iconTag);
        }
        $htmlString .= "</div>";
        return $htmlString;
    }

}

?>
