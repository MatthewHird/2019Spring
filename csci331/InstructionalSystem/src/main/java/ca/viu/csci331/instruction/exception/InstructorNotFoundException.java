package ca.viu.csci331.instruction.exception;

import ca.viu.csci331.instruction.model.Instructor;

public class InstructorNotFoundException extends Exception {

    private static final long serialVersionUID = 1L;

    public InstructorNotFoundException(Instructor instructor) {
        super("Instructor with id " + instructor.getInstructorId() + " not found");
    }
}
