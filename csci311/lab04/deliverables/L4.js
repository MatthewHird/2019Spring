// author: Matthew Hird
// csciId: hirdm
// date: Jan 13, 2019
// links:

function Calculator(numIn, operSel, numFld, operFld, resDis, formDis) {
    this.numInputEl = document.getElementById(numIn);
    this.operSelectEl = document.getElementById(operSel);
    this.numField = document.getElementById(numFld);
    this.operField = document.getElementById(operFld);
    this.resultDispEl = document.getElementById(resDis);
    this.formulaDispEl = document.getElementById(formDis);

    this.calcInputBorderColor = this.numInputEl.style.borderColor;
    this.formula = [];

    this.resetCalculator = function() { 
        this.formula = [];
        this.numInputEl.style.borderColor = this.calcInputBorderColor;
        this.numInputEl.value = '';
        this.operSelectEl.style.borderColor = this.calcInputBorderColor;
        this.operSelectEl.value = 'n/a';
        this.numField.removeAttribute('disabled');
        this.operField.disabled = 'disabled';
        this.resultDispEl.style.borderColor = this.calcInputBorderColor;
        this.resultDispEl.value = "";
        this.formulaDispEl.value = "";
    };

    this.pushNumField = function() {
        var newVal = this.numInputEl.value;
        if (isNaN(newVal) || newVal == "" || /\s/.test(newVal)) {
            this.numInputEl.style.borderColor = 'red';
        } else {
            this.numInputEl.style.borderColor = this.calcInputBorderColor;
            this.formula.push(Number(newVal));
            this.formulaDispEl.value = this.formulaDispEl.value + newVal;
            this.numField.disabled = 'disabled';
            this.operField.removeAttribute('disabled');
            this.numInputEl.value = '';
        }
    };

    this.pushOperField = function() {
        var newVal = this.operSelectEl.value;
        if (newVal == 'n/a') {
            this.operSelectEl.style.borderColor = 'red';
        } else {
            this.operSelectEl.style.borderColor = this.calcInputBorderColor;
            this.formula.push(newVal);
            this.formulaDispEl.value = this.formulaDispEl.value + newVal;
            this.operField.disabled = 'disabled';
            this.numField.removeAttribute('disabled');
            this.operSelectEl.value = 'n/a';
        }
    };

    this.calculateResult = function() {
        if (this.formula.length == 0) {
            return;
        } else if (this.formula.length % 2 == 0) {
            this.resultDispEl.style.borderColor = 'red';
            this.resultDispEl.value = 'Formula must not end in an operator';
        } else {
            this.resultDispEl.style.borderColor = this.calcInputBorderColor;
            var tempFormula = [];
            for (var i = 0; i < this.formula.length; i++) {
                tempFormula.push(this.formula[i]);
            }

            var operations = (tempFormula.length - 1) / 2;

            for (var i = 0; i < operations; i++) {
                var num1 = tempFormula.shift();
                var oper = tempFormula.shift();

                if (oper == '*') {
                    num1 = num1 * tempFormula.shift();
                    tempFormula.unshift(num1);
                } else if (oper == '/') {
                    num1 = num1 / tempFormula.shift();
                    tempFormula.unshift(num1);
                } else {
                    tempFormula.push(num1);
                    tempFormula.push(oper);
                }
            }
            tempFormula.push(tempFormula.shift());

            operations = (tempFormula.length - 1) / 2;
            
            for (var i = 0; i < operations; i++) {
                var num1 = tempFormula.shift();
                var oper = tempFormula.shift();

                if (oper == '+') {
                    num1 = num1 + tempFormula.shift();
                    tempFormula.unshift(num1);
                } else {
                    num1 = num1 - tempFormula.shift();
                    tempFormula.unshift(num1);
                } 
            }
            this.resultDispEl.value = tempFormula.shift().toString();            
        }
    };
}

var myCalc = new Calculator('numericInput', 'operatorDropdown', 'numericField', 
                            'operatorField', 'calcDisplay', 'currentFormula');
