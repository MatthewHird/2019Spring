<?php 
// author: Matthew Hird
// csciId: hirdm
// date: Apr 13, 2019

require_once('../csci370Resources/config.php');


function addProduct($productType, $productArray) {

    $query = array(
        '1' => "INSERT INTO ship (id, name, variant, faction, size, attack, agility, hull, shield) SELECT MAX(S.id + 1), :na, :va, :fa, :si, :at, :ag, :hu, :sh FROM ship S",
        
        '2' => "INSERT INTO huge_ship (ship_id, energy) SELECT S.id, :en FROM ship S WHERE S.name = :na AND S.variant = :va",
        
        '3' => "SELECT id FROM ship WHERE name = :xna and variant = :xva",

        '4' => "SELECT id FROM unique_name WHERE name = :xun",

        '5' => "INSERT INTO unique_name (id, name) SELECT MAX(UN.id + 1), :un FROM unique_name UN",
        
        '6' => "INSERT INTO pilot (id, name, variant, u_name_id, ship_id, pilot_skill, elite, point_cost) SELECT MAX(P.id + 1), :na, :va, :uid, :sid, :ps, :el, :pc FROM pilot P"
    );

    try {
        $conn = dbconnect();
        
        if ($productType == 'ships') {
            $conn->beginTransaction();
            
            $stmt1 = $conn->prepare($query['1']);

            $stmt1->bindParam(':na', $productArray['shipName']);
            $stmt1->bindParam(':va', $productArray['shipVariant']);
            $stmt1->bindParam(':fa', $productArray['faction']);
            $stmt1->bindParam(':si', $productArray['size']);
            $stmt1->bindParam(':at', $productArray['attack']);
            $stmt1->bindParam(':ag', $productArray['agility']);
            $stmt1->bindParam(':hu', $productArray['hull']);
            $stmt1->bindParam(':sh', $productArray['shield']);

            if (!$stmt1->execute()) {
                $conn->rollBack();
                $stmt1 = null;
                $conn = null;
                return array(false, "Failed to insert to 'ship' table");
            } 
            $stmt1 = null;

            if ($productArray['size'] == 'Huge') {
                $stmt2 = $conn->prepare($query['2']);

                $stmt2->bindParam(':en', $productArray['energy']);
                $stmt2->bindParam(':na', $productArray['shipName']);
                $stmt2->bindParam(':va', $productArray['shipVariant']);

                if (!$stmt2->execute()) {
                    $conn->rollBack();
                    $stmt2 = null;
                    $conn = null;
                    return array(false, "Failed to insert to 'huge_ship' table");
                } 
                $stmt2 = null;
            }

            $conn->commit();
            $conn = null;
            
            return array(true, "Ship '".$productArray['shipName'].($productArray['shipVariant'] ? ' ('.$productArray['shipVariant'].')' : '')."' successfully added");

        } else if ($productType == 'pilots') {
            $conn->beginTransaction();
            $stmt3 = $conn->prepare($query['3']);

            $stmt3->bindParam(':xna', $productArray['exShipName']);
            $stmt3->bindParam(':xva', $productArray['exShipVariant']);

            $stmt3->execute();
            $stmt3->bindColumn('id', $sid);
            if (!$stmt3->fetch(PDO::FETCH_BOUND)) {
                $conn->rollBack();
                $stmt3 = null;
                $conn = null;
                return array(false, "Existing ship name/variant not in 'ship' table");
            }
            $stmt3 = null;

            $stmt4 = $conn->prepare($query['4']);

            $stmt4->bindParam(':xun', $productArray['exUname']);
            $stmt4->execute();
            $stmt4->bindColumn('id', $uid);

            if (!$stmt4->fetch(PDO::FETCH_BOUND)) {
                $stmt5 = $conn->prepare($query['5']);
                $stmt5->bindParam(':un', $productArray['exUname']);
                if (!$stmt5->execute()) {
                    $conn->rollBack();
                    $stmt4 = null;
                    $stmt5 = null;
                    $conn = null;
                    return array(false, "Failed to find uname or insert new uname into 'unique_name' table");
                }
                $stmt5 = null;

                $stmt4->execute();
                $stmt4->bindColumn('id', $uid);
                $stmt4->fetch(PDO::FETCH_BOUND);
            }
            $stmt4 = null;
            
            $stmt6 = $conn->prepare($query['6']);
            
            $stmt6->bindParam(':na', $productArray['pilotName']);
            $stmt6->bindParam(':va', $productArray['pilotVariant']);
            $stmt6->bindParam(':uid', $uid);
            $stmt6->bindParam(':sid', $sid);
            $stmt6->bindParam(':ps', $productArray['pilotSkill']);
            $stmt6->bindParam(':el', $productArray['elite']);
            $stmt6->bindParam(':pc', $productArray['pointCost']);

            if (!$stmt6->execute()) {
                $conn->rollBack();
                $stmt6 = null;
                $conn = null;
                return array(false, "Failed to insert into 'pilot' table");
            }
            $stmt6 = null;

            $conn->commit();
            $conn = null;
            
            return array(true, "Pilot card '".$productArray['pilotName'].($productArray['pilotVariant'] ? ' ('.$productArray['pilotVariant'].')' : '')."' successfully added");

        } else {
            $conn = null;
            return array(false, "Incorrect product type.");
        }

    } catch(PDOException $e) {
        $conn = null;
        return "Database error<br />";
    }

    return "ok";
}


$err = "";

if (isset($_POST)) {
    if (!isset($_POST['productType'])) {
        $err .= "Valid product type required<br />";
    } else {
        $productType = $_POST['productType'];
        if ($productType != 'ships' && $productType != 'pilots') {
            $err .= "Valid product type required<br />"; 
        } else { 
            $productArray = array();
            if ($productType == 'ships') {
                if (!trim($_POST['shipName'])) {
                    echo "Product addition unsuccessful.<br />Ship name may not be left blank";
                    exit();
                } 

                $productArray['shipName'] = trim($_POST['shipName']);
                $productArray['shipVariant'] = trim($_POST['shipVariant']);
                $productArray['faction'] = trim($_POST['faction']);
                $productArray['size'] = trim($_POST['size']);

                if ($productArray['size'] == 'Huge') {
                    $productArray['energy'] = $_POST['energy1'] == 'val' ? $_POST['energy2'] : null;
                }

                $productArray['attack'] = $_POST['attack1'] == 'val' ? $_POST['attack2'] : null;
                $productArray['agility'] = $_POST['agility1'] == 'val' ? $_POST['agility2'] : null;
                $productArray['hull'] = $_POST['hull1'] == 'val' ? $_POST['hull2'] : null;
                $productArray['shield'] = $_POST['shield1'] == 'val' ? $_POST['shield2'] : null;
            } else {
                if (!trim($_POST['pilotName'])) {
                    echo "Product addition unsuccessful.<br />Pilot name may not be left blank";
                    exit();
                } 

                $productArray['pilotName'] = trim($_POST['pilotName']);
                $productArray['pilotVariant'] = trim($_POST['pilotVariant']);
                $productArray['pilotSkill'] = $_POST['pilotSkill'];
                $productArray['elite'] = $_POST['elite'];
                $productArray['pointCost'] = $_POST['pointCost1'] == 'val' ? $_POST['pointCost2'] : null;
                $productArray['exUname'] = trim($_POST['exUname']) ? trim($_POST['exUname']) : 'FALSE';
                $productArray['exShipName'] = trim($_POST['exShipName']);
                $productArray['exShipVariant'] = trim($_POST['exShipVariant']);
            }

            $confirm = addProduct($productType, $productArray);
            if ($confirm[0]) {
                echo "Product successfully added.<br />";
                echo $confirm[1];
            } else {
                echo "Product addition unsuccessful.<br />";
                echo $confirm[1];
            }
        }
    } 

} else {
    $err .= "No product to add<br />";
}

if (trim($err)) {
    echo $err;
}

exit();
?>