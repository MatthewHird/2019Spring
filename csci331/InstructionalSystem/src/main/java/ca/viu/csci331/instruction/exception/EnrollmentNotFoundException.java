package ca.viu.csci331.instruction.exception;


public class EnrollmentNotFoundException extends Exception {

    private static final long serialVersionUID = 1L;

    public EnrollmentNotFoundException(String studentId, String seminarId) {
        super(String.format("No enrollment found for student with ID %s and seminar with ID %s", studentId, seminarId));
    }
}
