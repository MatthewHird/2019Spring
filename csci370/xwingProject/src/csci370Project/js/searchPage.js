// author: Matthew Hird
// csciId: hirdm
// date: April 13, 2019

function disableFields() {
    let resultTypeSelect = document.getElementById("resultType");
    let resultType = resultTypeSelect[resultTypeSelect.selectedIndex].value;

    if (resultType == 'ships') {
        document.getElementById('pilotName').setAttribute("disabled", "disabled");
        document.getElementById('pilotSkill1').setAttribute("disabled", "disabled");
        document.getElementById('pilotSkill2').setAttribute("disabled", "disabled");
    } else {
        document.getElementById('pilotName').removeAttribute("disabled");
        document.getElementById('pilotSkill1').removeAttribute("disabled");
        document.getElementById('pilotSkill2').removeAttribute("disabled");
    }
}


disableFields();
