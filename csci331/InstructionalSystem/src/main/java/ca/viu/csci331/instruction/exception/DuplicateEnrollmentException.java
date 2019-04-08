package ca.viu.csci331.instruction.exception;


public class DuplicateEnrollmentException extends Exception {
    private static final long serialVersionUID = 1L;
    
    public DuplicateEnrollmentException(String studentId, String seminarId) {
        super(String.format("Enrollment with student ID %s and seminar ID %s already exists", 
                studentId, seminarId));
    }
}
