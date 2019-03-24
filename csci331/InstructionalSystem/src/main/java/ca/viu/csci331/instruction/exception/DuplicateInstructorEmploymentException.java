package ca.viu.csci331.instruction.exception;

import ca.viu.csci331.instruction.model.InstructorEmployment;

public class DuplicateInstructorEmploymentException extends Exception {
    private static final long serialVersionUID = 1L;
    private String duplicateInstructorId;
    
    public DuplicateInstructorEmploymentException(InstructorEmployment instructorEmployment) {
        super("InstructorEmployment with InstructorID \"" 
                + instructorEmployment.getInstructor().getInstructorId() + "\" already exists");
        duplicateInstructorId = instructorEmployment.getInstructor().getInstructorId();
    }
    
    public String getDuplicateInstructorId() {
        return duplicateInstructorId;
    }
}
