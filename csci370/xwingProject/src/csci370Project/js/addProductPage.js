// author: Matthew Hird
// csciId: hirdm
// date: April 13, 2019

function addProduct() {
    let productType = document.getElementById("productType").value;

    let reqBody = "productType=" + productType;

    if (productType == "ships") {
        reqBody += "&shipName=" + document.getElementById("shipName").value;
        reqBody += "&shipVariant=" + document.getElementById("shipVariant").value;
        reqBody += "&faction=" + document.getElementById("faction").value;
        shipSize = document.getElementById("size").value;
        reqBody += "&size=" + shipSize;

        if (shipSize == "Huge") {
            reqBody += "&energy1=" + document.getElementById("energy1").value;
            reqBody += "&energy2=" + document.getElementById("energy2").value;
        }

        reqBody += "&attack1=" + document.getElementById("attack1").value;
        reqBody += "&attack2=" + document.getElementById("attack2").value;
        reqBody += "&agility1=" + document.getElementById("agility1").value;
        reqBody += "&agility2=" + document.getElementById("agility2").value;
        reqBody += "&hull1=" + document.getElementById("hull1").value;
        reqBody += "&hull2=" + document.getElementById("hull2").value;
        reqBody += "&shield1=" + document.getElementById("shield1").value;
        reqBody += "&shield2=" + document.getElementById("shield2").value;

    } else if (productType == "pilots") {
        reqBody += "&pilotName=" + document.getElementById("pilotName").value;
        reqBody += "&pilotVariant=" + document.getElementById("pilotVariant").value;
        reqBody += "&pilotSkill=" + document.getElementById("pilotSkill").value;
        reqBody += "&elite=" + document.getElementById("elite").value;
        reqBody += "&pointCost1=" + document.getElementById("pointCost1").value;
        reqBody += "&pointCost2=" + document.getElementById("pointCost2").value;
        reqBody += "&exUname=" + document.getElementById("exUname").value;
        reqBody += "&exShipName=" + document.getElementById("exShipName").value;
        reqBody += "&exShipVariant=" + document.getElementById("exShipVariant").value;
    }

    var xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = function() {
        if (this.readyState == 4 && this.status == 200) {
            document.getElementById("confirmBox").innerHTML = this.responseText;
        }
    };
    xmlhttp.open("POST", "addProductHandler.php", true);
    xmlhttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    xmlhttp.send(reqBody);
}



function changeFields() {
    let productType = document.getElementById("productType").value;

    let shipSize = document.getElementById("size").value;


    if (productType == 'ships') {
        document.getElementById('pilotNameGroup').style.display = 'none';
        document.getElementById('pilotVariantGroup').style.display = 'none';
        document.getElementById('pilotSkillGroup').style.display = 'none';
        document.getElementById('eliteGroup').style.display = 'none';
        document.getElementById('pointCostGroup').style.display = 'none';
        document.getElementById('exUnameGroup').style.display = 'none';
        document.getElementById('exShipNameGroup').style.display = 'none';
        document.getElementById('exShipVariantGroup').style.display = 'none';

        document.getElementById('shipNameGroup').style.display = 'flex';
        document.getElementById('shipVariantGroup').style.display = 'flex';
        document.getElementById('factionGroup').style.display = 'flex';
        document.getElementById('sizeGroup').style.display = 'flex';
        
        if (shipSize == 'Huge') {
            document.getElementById('energyGroup').style.display = 'flex';
        } else {
            document.getElementById('energyGroup').style.display = 'none';
        }
        
        document.getElementById('attackGroup').style.display = 'flex';
        document.getElementById('agilityGroup').style.display = 'flex';
        document.getElementById('hullGroup').style.display = 'flex';
        document.getElementById('shieldGroup').style.display = 'flex';
    } else {
        document.getElementById('pilotNameGroup').style.display = 'flex';
        document.getElementById('pilotVariantGroup').style.display = 'flex';
        document.getElementById('pilotSkillGroup').style.display = 'flex';
        document.getElementById('eliteGroup').style.display = 'flex';
        document.getElementById('pointCostGroup').style.display = 'flex';
        document.getElementById('exUnameGroup').style.display = 'flex';
        document.getElementById('exShipNameGroup').style.display = 'flex';
        document.getElementById('exShipVariantGroup').style.display = 'flex';

        document.getElementById('shipNameGroup').style.display = 'none';
        document.getElementById('shipVariantGroup').style.display = 'none';
        document.getElementById('factionGroup').style.display = 'none';
        document.getElementById('sizeGroup').style.display = 'none';

        document.getElementById('attackGroup').style.display = 'none';
        document.getElementById('energyGroup').style.display = 'none';
        document.getElementById('agilityGroup').style.display = 'none';
        document.getElementById('hullGroup').style.display = 'none';
        document.getElementById('shieldGroup').style.display = 'none';
    }
}


changeFields();
