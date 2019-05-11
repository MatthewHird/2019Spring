<!-- 
// author: Matthew Hird
// csciId: hirdm
// date: April 13, 2019
-->
<?php

require_once('../csci370Resources/config.php');

$author_name = "Matthew Hird";

$stylesheets[] = 'xwingProject.css';
$jsImports[] = 'searchPage.js';


function handleGetRequest($get) {
    if (isset($get['resultType'])) {
        $resultType = trim($get['resultType']);
    } else {
        return false;
    }

    if ($resultType == 'ships') {
        $queryIndex = '1';
    } else if ($resultType == 'pilots') {
        $queryIndex = '2';
    } else {
        return false;
    }

    if (isset($get['pilotName'])) {
        $pilotName = trim($get['pilotName']);
    }
    if (isset($get['pilotSkill1']) && $get['pilotSkill1'] != 'blank' && isset($get['pilotSkill2'])) {
        $pilotSkill1 = parseComparitor($get['pilotSkill1']);
        $pilotSkill2 = trim($get['pilotSkill2']);
    }
    if (isset($get['shipName'])) {
        $shipName = trim($get['shipName']);
    }
    if (isset($get['nameContains']) && trim($get['nameContains'])) {
        if ($resultType == 'ships') {
            $shipName = trim($get['nameContains']);
        } else {
            $pilotName = trim($get['nameContains']);
        }
    }
    if (isset($get['faction']) && $get['faction'] != 'blank') {
        $faction = trim($get['faction']);
    }
    if (isset($get['size']) && $get['size'] != 'blank') {
        $shipSize = trim($get['size']);
    }
    if (isset($get['attack1']) && $get['attack1'] != 'blank' && isset($get['attack2'])) {
        $attack1 = parseComparitor($get['attack1']);
        $attack2 = trim($get['attack2']);
    }
    if (isset($get['agility1']) && $get['agility1'] != 'blank' && isset($get['agility2'])) {
        $agility1 = parseComparitor($get['agility1']);
        $agility2 = trim($get['agility2']);
    }
    if (isset($get['hull1']) && $get['hull1'] != 'blank' && isset($get['hull2'])) {
        $hull1 = parseComparitor($get['hull1']);
        $hull2 = trim($get['hull2']);
    }
    if (isset($get['shield1']) && $get['shield1'] != 'blank' && isset($get['shield2'])) {
        $shield1 = parseComparitor($get['shield1']);
        $shield2 = trim($get['shield2']);
    }

    $queryConditions = array();

    if ($queryIndex == '2') {
        if ($pilotName) {
            $queryConditions[] = array('P.name', 'LIKE', "%$pilotName%");
        }
        if ($pilotSkill1 && $pilotSkill2) {
            $queryConditions[] = array('P.pilot_skill', $pilotSkill1, $pilotSkill2);
        }
    }

    if ($shipName) {
        $queryConditions[] = array('S.name', 'LIKE', "%$shipName%");
    }
    if ($faction) {
        $queryConditions[] = array('S.faction', '=', $faction);
    }
    if ($shipSize) {
        if ($shipSize == 'Normal') {
            $queryConditions[] = array('S.size', '!=', 'Huge');
        } else {
            $queryConditions[] = array('S.size', '=', $shipSize);
        }
    }
    if ($attack1 && $attack2) {
        $queryConditions[] = array('S.attack', $attack1, $attack2);
    }
    if ($agility1 && ($agility2 || $agility2 == 0)) {
        $queryConditions[] = array('S.agility', $agility1, $agility2);
    }
    if ($hull1 && $hull2) {
        $queryConditions[] = array('S.hull', $hull1, $hull2);
    }
    if ($shield1 && ($shield2 || $shield2 == 0)) {
        $queryConditions[] = array('S.shield', $shield1 && $shield2);
    }

    $query = array(
        '1' => "SELECT S.id AS s_id, S.name AS s_name, S.variant AS s_variant, S.faction AS s_faction FROM ship S",
        '2' => "SELECT P.id AS p_id, P.name AS p_name, P.variant AS p_variant, S.id AS s_id, S.name AS s_name, S.variant AS s_variant, S.faction AS s_faction FROM ship S JOIN pilot P ON S.id = P.ship_id"
    );
    
    $searchQuery = $query[$queryIndex];

    if ($queryConditions) {
        $searchQuery .= sprintf(' WHERE %s %s ?', $queryConditions[0][0], $queryConditions[0][1]);

        for ($i=1; $i < sizeof($queryConditions); $i++) { 
            $searchQuery .= sprintf(' AND %s %s ?', $queryConditions[$i][0], $queryConditions[$i][1]);
        }
    }

    try {
        $conn = dbconnect();

        $stmt1 = $conn->prepare($searchQuery);

        for ($i=0; $i < sizeof($queryConditions); $i++) { 
            $stmt1->bindParam($i + 1, $queryConditions[$i][2]);
        }
        $stmt1->execute();
        $resultSet = $stmt1->fetchAll();
        $stmt1 = null;

        if (!$resultSet) {
            $stmt1 = null;
            $conn = null;
            return array(true, "<div class='standard-layout'><h1>Search Results</h1><hr><div>Sorry, No Results Found</div></div>");
        }
    } catch(PDOException $e) {
        $stmt1 = null;
        return array(false, "Error: ".$e->getMessage());
    }
    
    $conn = null;

    $rowTag = "<tr class='srt-row'>%s</tr>";
    $cellTag = "<td class='srt-cell'>%s</td>";

    $pilotLinkTag = "<a href='pilotPage.php?pilotId=%s'>%s</a>";
    $shipLinkTag = "<a href='shipPage.php?shipId=%s'>%s</a>";
    $factionLinkTag = "<a href='searchPage.php?resultType=ships&faction=%s'>%s</a>";

    $htmlString = "<div class='standard-layout'><h1>Search Results</h1><hr><div><div class='display-container-ib'><div class='search-result-div'><div class='srt'><table class='srt-table'>%s</table></div></div></div></div></div>";

    if ($queryIndex == 1) {
        $tableRows = "<tr class='srt-row'><th class='srt-cell'>Ship Name</th><th class='srt-cell'>Faction</th></tr>";
    } else {
        $tableRows = "<tr class='srt-row'><th class='srt-cell'>Pilot Name</th><th class='srt-cell'>Ship Name</th><th class='srt-cell'>Faction</th></tr>";
    }

    foreach ($resultSet as $key => $item) {
        $currentRow = "";
        $sName = sprintf($shipLinkTag, $item['s_id'], ($item['s_variant'] ? $item['s_name'].' ('.$item['s_variant'].')' : $item['s_name']));
        $sNameCell = sprintf($cellTag, $sName);
         
         $faction = sprintf($factionLinkTag, $item['s_faction'], $item['s_faction']);
         $factionCell = sprintf($cellTag, $faction);

        if ($queryIndex == 2) {
            $pName = sprintf($pilotLinkTag, $item['p_id'], ($item['p_variant'] ? $item['p_name'].' ('.$item['p_variant'].')' : $item['p_name']));
            $pNameCell = sprintf($cellTag, $pName);
            $currentRow .= $pNameCell;
        }
        $currentRow .= $sNameCell.$factionCell;
        $tableRows .= sprintf($rowTag, $currentRow);
    }

    $htmlString = sprintf($htmlString, $tableRows);
    
    return array('Results ', $htmlString);
}

function parseComparitor($inVal) {
    $temp = trim($inVal);
    
    if ($temp == 'eq') {
        return '=';
    } else if ($temp == 'gte') {
        return '>=';
    } else if ($temp == 'lte') {
        return '<=';
    }else if ($temp == 'ne') {
        return '!=';
    } else {
        return false;
    }
}


if ($getResult = handleGetRequest($_GET)) {
    $page_title = "Search ".$getResult[0]."Page - $siteTitle";
    $searchResultHtml = $getResult[1];
} 


// All stylesheets must be added to $stylesheets[] before importing 'head.php'
require_once(GEN_TEMPLATES."head.php");
require_once(GEN_TEMPLATES."header.php");

?>
<div class="form-wrapper">
    <form id="searchForm" data-abide novalidate method="get" action="searchPage.php">
        <div>
            <div id="resultTypeGroup" class="input-group">
                <label for="resultType" class="input-group-label search-label">Search for:</label>
                <select id="resultType" class="input-group-field" name="resultType" onchange="disableFields()">
                    <option value="ships" selected="selected">Ships</option>
                    <option value="pilots">Pilots</option>
                </select>
            </div>
            <div id="pilotNameGroup" class="input-group">
                <label for="pilotName" class="input-group-label search-label">Pilot Name:</label>
                <input id="pilotName" name='pilotName' class="input-group-field" type='text' placeholder='contains'>
            </div>
            <div id="pilotSkillGroup" class="input-group">
                <label for="pilotSkill1" class="input-group-label">Pilot Skill:</label>
                <select id="pilotSkill1" class="input-group-field" name="pilotSkill1">
                    <option value="blank">N/A</option>
                    <option value="eq">==</option>
                    <option value="gte">&gt;=</option>
                    <option value="lte">&lt;=</option>
                    <option value="ne">!=</option>
                </select>
                <input type="number" class="input-group-field" id="pilotSkill2" name="pilotSkill2" value="5" min="1" max="9">
            </div>
            <div id="shipNameGroup" class="input-group">
                <label for="shipName" class="input-group-label search-label">Ship Name:</label>
                <input id="shipName" name='shipName' class="input-group-field" type='text' placeholder='contains'>
            </div>
            <div id="factionGroup" class="input-group">
                <label for="faction" class="input-group-label search-label">Faction:</label>
                <select id="faction" class="input-group-field" name="faction">
                    <option value="blank">N/A</option>
                    <option value="Rebel">Rebel</option>
                    <option value="Imperial">Imperial</option>
                    <option value="Scum">Scum</option>
                </select>
            </div>
            <div id="sizeGroup" class="input-group">
                <label for="size" class="input-group-label search-label">Ship Size:</label>
                <select id="size" class="input-group-field" name="size">
                    <option value="blank">Any</option>
                    <option value="Normal">Not Huge</option>
                    <option value="Small">Small</option>
                    <option value="Large">Large</option>
                    <option value="Huge">Huge</option>
                </select>
            </div>
            <div id="attackGroup" class="input-group">
                <label for="attack1" class="input-group-label">Attack Value:</label>
                <select id="attack1" class="input-group-field" name="attack1">
                    <option value="blank">N/A</option>
                    <option value="eq">==</option>
                    <option value="gte">&gt;=</option>
                    <option value="lte">&lt;=</option>
                    <option value="ne">!=</option>
                </select>
                <input type="number" class="input-group-field" name="attack2" value="3" min="1" max="5">
            </div>
            <div id="agilityGroup" class="input-group">
                <label for="agility1" class="input-group-label">Agility Value:</label>
                <select id="agility1" class="input-group-field" name="agility1">
                    <option value="blank">N/A</option>
                    <option value="eq">==</option>
                    <option value="gte">&gt;=</option>
                    <option value="lte">&lt;=</option>
                    <option value="ne">!=</option>
                </select>
                <input type="number" class="input-group-field" name="agility2" value="2" min="0" max="3">
            </div>
            <div id="hullGroup" class="input-group">
                <label for="hull1" class="input-group-label">Hull Value:</label>
                <select id="hull1" class="input-group-field" name="hull1">
                    <option value="blank">N/A</option>
                    <option value="eq">==</option>
                    <option value="gte">&gt;=</option>
                    <option value="lte">&lt;=</option>
                    <option value="ne">!=</option>
                </select>
                <input type="number" class="input-group-field" name="hull2" value="3" min="2" max="12">
            </div>
            <div id="shieldGroup" class="input-group">
                <label for="shield1" class="input-group-label">Shield Value:</label>
                <select id="shield1" class="input-group-field" name="shield1">
                    <option value="blank">N/A</option>
                    <option value="eq">==</option>
                    <option value="gte">&gt;=</option>
                    <option value="lte">&lt;=</option>
                    <option value="ne">!=</option>
                </select>
                <input type="number" class="input-group-field" name="shield2" value="2" min="0" max="6">
            </div>
            <div>
                <button type='submit' class='button float-right'>Search</button>
            </div>
        </div>
    </form>
</div>

<?php 
    if (isset($searchResultHtml)) {
        echo $searchResultHtml;
    }
    require_once(GEN_TEMPLATES."footer.php"); 
?>
