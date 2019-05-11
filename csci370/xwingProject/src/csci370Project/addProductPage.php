<!-- 
// author: Matthew Hird
// csciId: hirdm
// date: April 13, 2019
-->
<?php
require_once('../csci370Resources/config.php');

$author_name = "Matthew Hird";

$stylesheets[] = 'xwingProject.css';
$jsImports[] = 'addProductPage.js';

// All stylesheets must be added to $stylesheets[] before importing 'head.php'
require_once(GEN_TEMPLATES."head.php");
require_once(GEN_TEMPLATES."header.php");
?>
<div id="addProductMain">
    <div class="form-wrapper inline-block">
        <form id="addProductForm" data-abide novalidate method="get" action="searchPage.php">
            <div>
                <div id="productTypeGroup" class="input-group">
                    <label for="productType" class="input-group-label search-label">Add new:</label>
                    <select id="productType" class="input-group-field" name="productType" onchange="changeFields()">
                        <option value="ships" selected="selected">Ships</option>
                        <option value="pilots">Pilots</option>
                    </select>
                </div>
                


                <div id="pilotNameGroup" class="input-group">
                    <label for="pilotName" class="input-group-label search-label">Pilot Name:</label>
                    <input id="pilotName" name='pilotName' class="input-group-field" type='text' placeholder='new pilot name' required>
                </div>
                <div id="pilotVariantGroup" class="input-group">
                    <label for="pilotVariant" class="input-group-label search-label">Pilot Variant:</label>
                    <input id="pilotVariant" name='pilotVariant' class="input-group-field" type='text' placeholder='new pilot variant'>
                </div>
                <div id="pilotSkillGroup" class="input-group">
                    <label for="pilotSkill" class="input-group-label">Pilot Skill:</label>
                    <input type="number" class="input-group-field" id="pilotSkill" name="pilotSkill" value="5" min="-1" max="9">
                </div>
                <div id="eliteGroup" class="input-group">
                    <label for="elite" class="input-group-label search-label">Elite:</label>
                    <select id="elite" class="input-group-field" name="elite">
                        <option value="0">No</option>
                        <option value="1">Yes</option>
                    </select>
                </div>
                <div id="pointCostGroup" class="input-group">
                    <label for="pointCost1" class="input-group-label">Point Cost:</label>
                    <select id="pointCost1" class="input-group-field" name="pointCost1">
                        <option value="val">value</option>
                        <option value="blank">N/A</option>
                    </select>
                    <input type="number" class="input-group-field" id="pointCost2" name="pointCost2" value="22" min="12" max="99">
                </div>

                <div id="exUnameGroup" class="input-group">
                    <label for="exUname" class="input-group-label search-label">Existing Unique Name:</label>
                    <input id="exUname" name='exUname' class="input-group-field" type='text' placeholder='existing unique name'>
                </div>
                <div id="exShipNameGroup" class="input-group">
                    <label for="exShipName" class="input-group-label search-label">Existing Ship Name:</label>
                    <input id="exShipName" name='exShipName' class="input-group-field" type='text' placeholder='existing ship name' required>
                </div>
                <div id="exShipVariantGroup" class="input-group">
                    <label for="exShipVariant" class="input-group-label search-label">Existing Ship Variant:</label>
                    <input id="exShipVariant" name='exShipVariant' class="input-group-field" type='text' placeholder='existing variant name'>
                </div>



                <div id="shipNameGroup" class="input-group">
                    <label for="shipName" class="input-group-label search-label">Ship Name:</label>
                    <input id="shipName" name='shipName' class="input-group-field" type='text' placeholder='new ship name' required>
                </div>
                <div id="shipVariantGroup" class="input-group">
                    <label for="shipVariant" class="input-group-label search-label">Ship Variant:</label>
                    <input id="shipVariant" name='shipVariant' class="input-group-field" type='text' placeholder='new ship variant'>
                </div>

                <div id="factionGroup" class="input-group">
                    <label for="faction" class="input-group-label search-label">Faction:</label>
                    <select id="faction" class="input-group-field" name="faction">
                        <option value="Rebel">Rebel</option>
                        <option value="Imperial">Imperial</option>
                        <option value="Scum">Scum</option>
                    </select>
                </div>
                <div id="sizeGroup" class="input-group">
                    <label for="size" class="input-group-label search-label">Ship Size:</label>
                    <select id="size" class="input-group-field" name="size" onchange="changeFields()">
                        <option value="Small">Small</option>
                        <option value="Large">Large</option>
                        <option value="Huge">Huge</option>
                    </select>
                </div>

                <div id="attackGroup" class="input-group">
                    <label for="attack1" class="input-group-label">Attack Value:</label>
                    <select id="attack1" class="input-group-field" name="attack1">
                        <option value="val">value</option>
                        <option value="blank">N/A</option>
                    </select>
                    <input type="number" class="input-group-field" id="attack2" name="attack2" value="3" min="0" max="9">
                </div>
                <div id="energyGroup" class="input-group">
                    <label for="energy1" class="input-group-label">Energy Value:</label>
                    <select id="energy1" class="input-group-field" name="energy1">
                        <option value="blank">N/A</option>
                        <option value="val">value</option>
                    </select>
                    <input type="number" class="input-group-field" id="energy2" name="energy2" value="4" min="0" max="99">
                </div>
                <div id="agilityGroup" class="input-group">
                    <label for="agility1" class="input-group-label">Agility Value:</label>
                    <select id="agility1" class="input-group-field" name="agility1">
                        <option value="val">value</option>
                        <option value="blank">N/A</option>
                    </select>
                    <input type="number" class="input-group-field" id="agility2" name="agility2" value="2" min="0" max="9">
                </div>
                <div id="hullGroup" class="input-group">
                    <label for="hull1" class="input-group-label">Hull Value:</label>
                    <select id="hull1" class="input-group-field" name="hull1">
                        <option value="val">value</option>
                        <option value="blank">N/A</option>
                    </select>
                    <input type="number" class="input-group-field" id="hull2" name="hull2" value="3" min="1" max="99">
                </div>
                <div id="shieldGroup" class="input-group">
                    <label for="shield1" class="input-group-label">Shield Value:</label>
                    <select id="shield1" class="input-group-field" name="shield1">
                        <option value="val">value</option>
                        <option value="blank">N/A</option>
                    </select>
                    <input type="number" class="input-group-field" id="shield2" name="shield2" value="2" min="0" max="99">
                </div>
                <div>
                    <button type='button' class='button float-right' onclick='addProduct()'>Add Product</button>
                </div>
            </div>
        </form>
    </div>
    <div class="standard-layout" style="vertical-align: top;">
        <div id="confirmHeader">
            <h2>Confirmation Message Box:</h2>
            <hr>
        </div>
        <div id="confirmBox"></div>
    </div>
</div>

<?php require_once(GEN_TEMPLATES."footer.php"); ?>
