package ca.viu.csci331.instruction.exception;

import ca.viu.csci331.instruction.model.StudentAdmission;

public class DuplicateStudentAdmissionException extends Exception {
    private static final long serialVersionUID = 1L;

    public DuplicateStudentAdmissionException(StudentAdmission studentAdmission) {
        super("StudentAdmission with StudentID \"" 
                + studentAdmission.getStudent().getStudentId() + "\" already exists");
    }
}
