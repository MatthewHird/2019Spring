package ca.viu.csci331.lab1;

import static org.junit.Assert.*;

import org.junit.Test;
public class GreetTests {

	@Test
	public void testGetText() {
		String textString1 = "Hello World!";
		String textString2 = "Different text";
		Greet greeter = new Greet(textString1);
		
		/**
		 * Test that getText() works
		 */
		assertTrue(textString1.equals(greeter.getText()));

		/**
		 * Test that setText(String) method works
		 */
		greeter.setText(textString2);
		assertTrue(textString2.equals(greeter.getText()));
	}
}
