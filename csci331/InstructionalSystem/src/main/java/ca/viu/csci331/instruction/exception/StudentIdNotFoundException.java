package ca.viu.csci331.instruction.exception;

public class StudentIdNotFoundException extends Exception {

    private static final long serialVersionUID = 1L;

    public StudentIdNotFoundException(String studentId) {
        super(studentId + " not found");
    }
}
