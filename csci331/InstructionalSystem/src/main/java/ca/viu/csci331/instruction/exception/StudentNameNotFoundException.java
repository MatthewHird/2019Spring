package ca.viu.csci331.instruction.exception;

public class StudentNameNotFoundException extends Exception {
    private static final long serialVersionUID = 1L;

    public StudentNameNotFoundException(String studentName) {
        super("No students found with name \"" + studentName + "\"");
    }
}
