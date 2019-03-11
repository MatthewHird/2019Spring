package ca.viu.csci331.instruction.exception;

public class InvalidStudentIdSyntaxException extends Exception {
    private static final long serialVersionUID = 1L;
    
    public InvalidStudentIdSyntaxException(String invalidStudentId) {
        super(String.format("StudentId %s is invalid: Must be 9 characters long, only include\n"
                + "characters between 0-9 and not start start with 0",
                invalidStudentId));
    }

}
