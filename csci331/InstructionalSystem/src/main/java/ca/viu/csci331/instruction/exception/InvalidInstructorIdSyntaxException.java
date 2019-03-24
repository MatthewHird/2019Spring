package ca.viu.csci331.instruction.exception;

public class InvalidInstructorIdSyntaxException extends Exception {
    private static final long serialVersionUID = 1L;
    
    public InvalidInstructorIdSyntaxException(String invalidInstructorId) {
        super(String.format("InstructorId %s is invalid: Must be 6 characters long, only include\n"
                + "characters between 0-9 and not start start with 0",
                invalidInstructorId));
    }

}
