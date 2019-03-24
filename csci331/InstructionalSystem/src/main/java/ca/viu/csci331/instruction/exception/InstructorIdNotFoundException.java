package ca.viu.csci331.instruction.exception;

public class InstructorIdNotFoundException extends Exception {

    private static final long serialVersionUID = 1L;

    public InstructorIdNotFoundException(String instructorId) {
        super("Instructor with id " + instructorId + " not found");
    }
}
