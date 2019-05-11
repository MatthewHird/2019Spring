// author: Matthew Hird
// csciId: hirdm
// date: Jan 13, 2019
// links: 

//when div is clicked, move the div to a new random location
function moveMe() {
	// Code standards error. Declaring multiple variables on a single line is poor practice.
	// Also in this situation, x and y whould be declared and instantiated in one step as these
	// are the first 3 lines of the funtion.
	
	// Logic/syntax error. "Math.random()" returns a float value between 0 and 1. This
	// means x and y will also be floats. The "top" and "left" attributes of the element
	// must be set using length values. Length values need a unit of some sort and have
	// to be written as a string (ex. "200px"). This means that x and y need to be converted
	// strings using the ".toString()" method, then concatenated with the appropriate units.
	// Some length units must be with integer values, such as pixels. For these x and y would 
	// need to be truncated first before converting it to a string. In this case, I'd suggest 
	// truncating x and y, converting them to strings, then concatinating then with "px".
	var xPos = Math.floor(Math.random() * (window.innerWidth - 200)).toString();
	// Code standards. There is no space between the operators and operands. Add a single space
	// between the operators and operands.
	var yPos = Math.floor(Math.random() * 200).toString();
	// Code standards error. Although this technically works in this case, it is awkward and 
	// confusing to read, and will no longer work if other "square" class elements are added
	// to the Lab4.html document. The square element should be given an id attribute and 
	// should be accessed by its unique id instead of using a collection selector and selecting
	// it by index.
	var squareElement = document.getElementById("randomSquare");
	squareElement.style.top = yPos + "px";
	squareElement.style.left = xPos + "px";
	// Code standards error. "el" is a terrible variable name and should be replaced by 
	// something more descriptive like "squareElement".
}

// Code standards. Variables should be declared locally. Variables shouldn't be declared
// globally without good reason. Instead declare x and y within the "compute()" function.

//should compute the value of xVal + yVal and put the answer in the readonly
//text input location
function compute() {
	// Logic/syntax error. "xVal", "yVal" and "answer" are all <input> name attribute values, 
	// not class values. Should get elements by id instead, after the <inputs> are given 
	// unique id values in "Lab4.html". 
	var xVal = document.getElementById("xValue").value;
	var yVal = document.getElementById("yValue").value;
	// x and y are strings, so adding them together just concatinates the values. Instead, they
	// need to be converted into numbers, then added together. That result should then be 
	// converted into a string and used to set the value of xYAnswer.
	var xPlusY = parseInt(xVal) + parseInt(yVal);
	if (isNaN(xPlusY)) {
		document.getElementById("xYAnswer").value = "";
	} else {
		document.getElementById("xYAnswer").value = xPlusY.toString();
	}
}

//should be called on load of page
//opens a prompt to get user's name to customize page

// Code standards error. "doAThing()" is a terrible name for a function. Give it a more
// descriptive name like "namePrompt()". "name" and "el" aren't great names either.
function namePrompt() {
    // Security error. User entered text in inserted directly into document without sanitizing 
    // it. This leaves page vulnerable to malicious users who want to insert html code (though
    // the risks are lessened because the code is only used client side). I will not correct
    // this now because we haven't learned about this in class yet.
    var nameEntered = window.prompt("Hi there, what's your name?", "Thor");
    var userNameElement = document.getElementById("userName");
    userNameElement.innerHTML = "Hi " + nameEntered + ", welcome!";
}

//gets the colour from the colour picker, and uses it to set the text colour
//of the div with the lorem ipsum text
function changeColour() {
    // Unneccessary variables. document.getElementById("myColor").value; would 
    // give color value in a single statement. "col" could be declared and 
    // assigned a value in one step. Poor variable names. change "col" to "userColor". 
    var userColor = document.getElementById("myColor").value;
    // Logical error/documentation error. Sets background-color when comment says 
    // it should set text color. Change ".backgroundColor" to ".color"
    document.getElementById("loremText").style.color = userColor;
}