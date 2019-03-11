package ca.viu.csci331.instruction.exception;

public class InvalidAdmissionListTextFileSyntaxException extends Exception {
    private static final long serialVersionUID = 1L;

    public InvalidAdmissionListTextFileSyntaxException(String invalidSyntaxType) {
        super("Invalid syntax: " + invalidSyntaxType + "\n");
    }
}
