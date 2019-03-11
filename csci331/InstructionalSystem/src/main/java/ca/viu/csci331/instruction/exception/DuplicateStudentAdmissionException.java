package ca.viu.csci331.instruction.exception;

import ca.viu.csci331.instruction.model.StudentAdmission;

public class DuplicateStudentAdmissionException extends Exception {
    private static final long serialVersionUID = 1L;
    private String duplicateStudentId;
    
    public DuplicateStudentAdmissionException(StudentAdmission studentAdmission) {
        super("StudentAdmission with StudentID \"" 
                + studentAdmission.getStudent().getStudentId() + "\" already exists");
        duplicateStudentId = studentAdmission.getStudent().getStudentId();
    }
    
    public String getDuplicateStudentId() {
        return duplicateStudentId;
    }
}
