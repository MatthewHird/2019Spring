// author: Matthew Hird
// csciId: hirdm
// date: Apr 1, 2019
// links: 

function setFocus(event) {
    let element = event.currentTarget;
    document.getElementById(colourList[ccellFocus].id).style.boxShadow = "none";
    document.getElementById(colourList[ccellFocus].id).style.border = document.getElementById(colourList[ccellFocus].id).style.backgroundColor;

    ccellFocus = parseInt(element.id.charAt(5)) - 1;
    element = document.getElementById(colourList[ccellFocus].id);
    let currentColour = w3color(element.style.backgroundColor);

    if (currentColour.isDark()) {
        element.style.boxShadow = "0 -2px 0 black, 0 2px 0 black"; 
        element.style.border = "2px solid white"; 
    } else {
        element.style.boxShadow = "0 -2px 0 white, 0 2px 0 white"; 
        element.style.border = "2px solid black"; 
    }
}

function setLock(event) {
    let element = event.currentTarget;
    element.innerHTML = "<i class='fi-lock'>";
    element.style.opacity = 1.0;
    element.onclick = setUnlock;
    colourList[parseInt(element.id.charAt(5)) - 1].locked = "locked";
}

function setUnlock(event) {
    let element = event.currentTarget;
    element.innerHTML = "<i class='fi-unlock'>";
    element.style.opacity = 0.3;
    element.onclick = setLock;
    colourList[parseInt(element.id.charAt(5)) - 1].locked = "unlocked";
}

function undo() {

}

function redo() {
    
}

function randomAll() {
    for(let i = 0; i < 5; i++) {
        if (colourList[i].locked == "unlocked") {
            // statement
            let greyVal = Math.floor(Math.random() * 255);

            let c = w3color("rgb("+greyVal+","+greyVal+","+greyVal+")");
            document.getElementById(colourList[i].text).innerHTML = c.toHexString().toUpperCase();
            document.getElementById(colourList[i].id).style.backgroundColor = c.toRgbString();
            
            let other = "black";
            let other2 = "white";
            if (c.isDark()) {
                other = "white";
                other2 = "black";
            }
            
            document.getElementById(colourList[i].text).style.color =  other;
            document.getElementById(colourList[i].lock).style.color =  other;
            
            if (ccellFocus == i) {
                document.getElementById(colourList[i].id).style.border = "2px solid " + other; 
                document.getElementById(colourList[i].id).style.boxShadow = "0 -2px 0 " + other2 + ", 0 2px 0 " + other2; 
            }
        }
    }
}

function lockAll() {
    for(let i = 0; i < ccellLocks.length; i++) {
        let element = ccellLocks.item(i);
        element.innerHTML = "<i class='fi-lock'>";
        element.style.opacity = 1.0;
        element.onclick = setUnlock;
        colourList[i].locked = "locked";
    }
}

function unlockAll() {
    for(let i = 0; i < ccellLocks.length; i++) {
        let element = ccellLocks.item(i);
        element.innerHTML = "<i class='fi-unlock'>";
        element.style.opacity = 0.3;
        element.onclick = setLock;
        colourList[i].locked = "unlocked";   
    }
}


function hexInput(event) {
    if (event.keyCode === 13) {
        let element = event.currentTarget;
        colorInput(element.value);   
    }
}

function rgbInput(event) {
    if (event.keyCode === 13) {
        let element = event.currentTarget;

        let color = "rgb(" + element.value + ")";
        colorInput(color);   
    }
}

function colorInput(color) {
    // let vals = element.value.replace(/ /g, "").split(",");        
    // if (parseInt(vals[0]) != parseInt(vals[1]) || parseInt(vals[0]) != parseInt(vals[2])) {
    //     return;
    // }


    let c = w3color(color);
    if (c.valid) {
        if (c.red != c.green || c.red != c.blue) {
            document.getElementById("submitConfirmation").innerHTML = "Colour value must be grey<br />(red = green = blue)";
        } else {
            if (colourList[ccellFocus].locked == "unlocked") {
                document.getElementById(colourList[ccellFocus].text).innerHTML = c.toHexString().toUpperCase();
                document.getElementById(colourList[ccellFocus].id).style.backgroundColor = c.toRgbString();
                document.getElementById("cpwRGBInput").value = c.red+","+c.green+","+c.blue;
                document.getElementById("cpwHexInput").value = c.toHexString().toUpperCase();

                let other = "black";
                let other2 = "white";

                if (c.isDark()) {
                    other = "white";
                    other2 = "black";
                }
                    
                document.getElementById(colourList[ccellFocus].text).style.color = other;
                document.getElementById(colourList[ccellFocus].lock).style.color = other;
                
                document.getElementById(colourList[ccellFocus].id).style.boxShadow = "0 -2px 0 " + other2 + ", 0 2px 0 " + other2;  
                document.getElementById(colourList[ccellFocus].id).style.border = "solid 1px " + other; 
            } else {
                document.getElementById("submitConfirmation").innerHTML = "Cannot change colour:<br />Selected cell is locked";
            }
        }
    }
}


function savePalette() {
    let reqBody = "pname=" + document.getElementById("cpwPaletteName").value;
    for(let i = 0; i < 5; i++) {
        let c = w3color(document.getElementById(colourList[i].id).style.backgroundColor);
        reqBody += "&c" + (i + 1) + "r=" + c.red + "&c" + (i + 1) + "g=" + c.green 
                + "&c" + (i + 1) + "b=" + c.blue; 
    }

    var xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = function() {
        if (this.readyState == 4 && this.status == 200) {
            document.getElementById("submitConfirmation").innerHTML = this.responseText;
        }
    };
    xmlhttp.open("POST", "savePalette.php", true);
    xmlhttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    xmlhttp.send(reqBody);
}



var ccellFocus = 0;

var ccellLocks = document.getElementsByClassName("cpw-ccell-lock");

for(let i = 0; i < ccellLocks.length; i++) {
    ccellLocks.item(i).onclick = setLock;
}

var colourCells = document.getElementsByClassName("cpw-colour-cell");

for(let i = 0; i < colourCells.length; i++) {
    colourCells.item(i).onclick = setFocus;
}

document.getElementById("cpwRGBInput").addEventListener('keyup', rgbInput);
document.getElementById("cpwHexInput").addEventListener('keyup', hexInput);


var colourList = new Array();

for(let i = 0; i < 5; i++) {
    let num = i + 1;
    let myColour = {id:"ccell" + num, lock:"ccell" + num + "Lock", 
            text:"ccell" + num + "Text", locked:"unlocked"};
    colourList[i] = myColour;
}

randomAll();


