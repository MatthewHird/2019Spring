// author: Matthew Hird
// csciId: hirdm
// date: Mar 2, 2019
// links:

class Transformer {
    constructor(transformerId, textDisplayId) {
        this.transformerId = transformerId;
        this.textDisplayId = textDisplayId;
        this.radioGroupName = document.querySelector("#" + this.transformerId
                + " .transformImageRadioControl input").name;
        this.imageDisplayId = document.querySelector("#" + this.transformerId 
                + " .imageDisplay").id;
        this.transformImageTypeFieldId = document.querySelector("#" + this.transformerId 
                + " .transformImageType input").id;
        this.wordFieldId = document.querySelector("#" + this.transformerId 
                + " .transformDivTextWord input").id;
        this.colourSelector1 = document.querySelector("#" + this.transformerId 
                + " .colourSelector1 select").id;
        this.colourSelector2 = document.querySelector("#" + this.transformerId 
                + " .colourSelector2 select").id;
        this.skewFieldId = document.querySelector("#" + this.transformerId
                + " .transformDivTextSkew input").id;
        this.isTransformed = false;
    }

    displayImage() {
        let radioGroup = document.getElementsByName(this.radioGroupName);
        for (var i = 0; i < radioGroup.length; i++) {
            
            if (radioGroup[i].checked) {
                document.getElementById(radioGroup[i].value).style.display = "inline-block";
            } else {
                this.isTransformed = false;
                document.getElementById(radioGroup[i].value).style.display = "none";
                document.getElementById(radioGroup[i].value).style.transform = "rotateX(0deg)";
            }
        }
    }

    goTransformImage() {
        if (!this.isTransformed) {
            let tiType = document.getElementById(this.transformImageTypeFieldId).value.toUpperCase();

            let displayedImage;
            let radioGroup = document.getElementsByName(this.radioGroupName);
            for (var i = 0; i < radioGroup.length; i++) {                
                if (radioGroup[i].checked) {
                    displayedImage = document.getElementById(radioGroup[i].value)
                } 
            }

            if (tiType == "2D") {
                this.isTransformed = true;
                document.querySelector("#" + this.transformerId 
                        + " .transformImageType .invalidTransformationType").innerHTML = "";
                displayedImage.style.transition = "all 2s";
                displayedImage.style.transform = "rotate(180deg)";
            } else if (tiType == "3D") {
                this.isTransformed = true;
                document.querySelector("#" + this.transformerId 
                        + " .transformImageType .invalidTransformationType").innerHTML = "";
                displayedImage.style.transition = "all 2s";
                displayedImage.style.transform = "rotateX(180deg)";
            } else {
                document.querySelector("#" + this.transformerId 
                        + " .transformImageType .invalidTransformationType").innerHTML 
                        = "Type must be 2D or 3D to perform transformation";
            }
        }
    }

    resetTransformImage() {
        if (this.isTransformed) {
            let tiType = document.getElementById(this.transformImageTypeFieldId).value.toUpperCase();

            let displayedImage;
            let radioGroup = document.getElementsByName(this.radioGroupName);
            for (var i = 0; i < radioGroup.length; i++) {                
                if (radioGroup[i].checked) {
                    displayedImage = document.getElementById(radioGroup[i].value)
                } 
            }

            this.isTransformed = false;
            displayedImage.style.transition = "all 2s";
            displayedImage.style.transform = "rotateX(0deg)";
        }
    }

    addText() {
        let wordValue = document.getElementById(this.wordFieldId).value;
        if (wordValue.search(/\b[a-z]{3,12}\b/i) > -1) {
            let textString = "";
            for(let i = 0; i < 52; i++){
                textString += wordValue + " ";
            }
            document.getElementById(this.textDisplayId).innerHTML = textString;
            document.querySelector("#" + this.transformerId 
                    + " .transformDivTextWord .invalidWord").innerHTML = "";
        } else {
            document.querySelector("#" + this.transformerId 
                    + " .transformDivTextWord .invalidWord").innerHTML 
                    = "Word can contain upper and lower case letters only and must be"
                    + " between 3-12 characters long";
        }
    }

    clearText() {
        document.querySelector("#" + this.transformerId 
                + " .transformDivTextWord .invalidWord").innerHTML = ""; 
        document.getElementById(this.textDisplayId).style.transition = "all 0s";
        document.getElementById(this.textDisplayId).innerHTML = "";
        document.getElementById(this.textDisplayId).style.backgroundColor = "initial";
        document.getElementById(this.textDisplayId).style.transform = "skewX(0deg)";
    }

    colour1Update() {
        let cs1Options = document.querySelectorAll("#" + this.colourSelector1 + " option");
        let cs2Options = document.querySelectorAll("#" + this.colourSelector2 + " option");
        let selectedValue = "";
        for(let i = 0, length1 = cs1Options.length; i < length1; i++) {
            if (cs1Options[i].value == "blank") {
                if (!cs1Options[i].selected) {
                    cs1Options[i].setAttribute("hidden", "hidden");
                    document.getElementById(this.colourSelector2).removeAttribute("disabled");
                }
            } else if (cs1Options[i].selected) {
                selectedValue = cs1Options[i].value;
                document.getElementById(this.textDisplayId).style.transition = "all 2s";
                document.getElementById(this.textDisplayId).style.backgroundColor = selectedValue;
            }    
        }

        for(let i = 0, length1 = cs2Options.length; i < length1; i++) {
            if (cs2Options[i].value == selectedValue) {
                cs2Options[i].setAttribute("hidden", "hidden");
            } else if (cs2Options[i].value != "blank") {
                cs2Options[i].removeAttribute("hidden");
            }
        }
    }

    colour2Update() {
        let cs1Options = document.querySelectorAll("#" + this.colourSelector1 + " option");
        let cs2Options = document.querySelectorAll("#" + this.colourSelector2 + " option");
        let selectedValue = "";
        for(let i = 0, length1 = cs2Options.length; i < length1; i++) {
            if (cs2Options[i].value == "blank") {
                if (!cs2Options[i].selected) {
                    cs2Options[i].setAttribute("hidden", "hidden");
                }
            } else if (cs2Options[i].selected) {
                selectedValue = cs2Options[i].value;
            }    
        }

        for(let i = 0, length1 = cs1Options.length; i < length1; i++) {
            if (cs1Options[i].value == selectedValue) {
                cs1Options[i].setAttribute("hidden", "hidden");
            } else if (cs1Options[i].value != "blank") {
                cs1Options[i].removeAttribute("hidden");
            }
        }
    }

    textTransform() {
        let cs1Options = document.querySelectorAll("#" + this.colourSelector1 + " option");
        let cs2Options = document.querySelectorAll("#" + this.colourSelector2 + " option");
        let displayedText = document.getElementById(this.textDisplayId);
        let selectedValue1 = "blank";
        let selectedValue2 = "blank";
        let skewValue = NaN;

        for(let i = 0, length1 = cs1Options.length; i < length1; i++) {
            if (cs1Options[i].selected) {
                selectedValue1 = cs1Options[i].value;
            }    
        }

        for(let i = 0, length1 = cs2Options.length; i < length1; i++) {
            if (cs2Options[i].selected) {
                selectedValue2 = cs2Options[i].value;
            }
        }

        skewValue = parseInt(document.getElementById(this.skewFieldId).value);

        if (selectedValue1 == "blank") {
            document.querySelector("#" + this.transformerId 
                    + " .colourSelector1  .invalidColour1").innerHTML 
                    = "Colour 1 and Colour 2 must have colours selected";
        } else if (selectedValue2 == "blank") {
            document.querySelector("#" + this.transformerId 
                    + " .colourSelector1  .invalidColour1").innerHTML = "";
            document.querySelector("#" + this.transformerId 
                    + " .colourSelector2  .invalidColour2").innerHTML  
                    = "Colour 2 must have a colour selected";
        } else if(skewValue < 0 || skewValue > 360) {
            document.querySelector("#" + this.transformerId 
                    + " .colourSelector1  .invalidColour1").innerHTML = "";
            document.querySelector("#" + this.transformerId 
                    + " .colourSelector2  .invalidColour2").innerHTML = "";
            document.querySelector("#" + this.transformerId 
                    + " .transformDivTextSkew  .invalidSkew").innerHTML
                    = "Skew value must be between 0 and 360 degrees";
        } else {
            document.querySelector("#" + this.transformerId 
                    + " .colourSelector1  .invalidColour1").innerHTML = "";
            document.querySelector("#" + this.transformerId 
                    + " .colourSelector2  .invalidColour2").innerHTML = "";
            document.querySelector("#" + this.transformerId 
                    + " .transformDivTextSkew  .invalidSkew").innerHTML = "";
            displayedText.style.transition = "all 2s";
            displayedText.style.backgroundColor = selectedValue2;
            displayedText.style.transform = "skewX(" + skewValue.toString() + "deg)";
        }
    }

    resetTransformer() {
        document.getElementById(this.transformerId).reset();
        this.clearText();
        this.displayImage();
        document.getElementById(this.colourSelector2).setAttribute("disabled", "disabled");
        
        document.querySelector("#" + this.transformerId 
                + " .transformImageType .invalidTransformationType").innerHTML = "";
        document.querySelector("#" + this.transformerId 
                + " .colourSelector1  .invalidColour1").innerHTML = "";
        document.querySelector("#" + this.transformerId 
                + " .colourSelector2  .invalidColour2").innerHTML = "";
        document.querySelector("#" + this.transformerId 
                + " .transformDivTextSkew  .invalidSkew").innerHTML = "";
    }
}
