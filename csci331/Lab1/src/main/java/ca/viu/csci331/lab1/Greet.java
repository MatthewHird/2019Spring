package ca.viu.csci331.lab1;

public class Greet {
	private String text;
	
	public Greet() {
		text = "Hello human!";
	}
	
	public Greet(String inText) {
		text = inText;
	}
	
	public String getText() {
		return text;
	}
	public void setText(String inText) {
		text = inText;
	}
}
