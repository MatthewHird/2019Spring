//when div is clicked, move the div to a new random location
function moveMe(){
	var x, y;
	x = Math.random()*(window.innerWidth - 200);
	y = Math.random()*200;
	var el = document.getElementsByClassName("square")[0];
	el.style.top = y;
	el.style.left = x;
	
}
var x, y;
//should compute the value of xVal + yVal and put the answer in the readonly
//text input location
function compute(){
	x = document.getElementsByClassName("xVal")[0].value;
	y = document.getElementsByClassName("yVal")[0].value;
	var ans = x + y;
	document.getElementsByClassName("answer")[0].value = ans;
}