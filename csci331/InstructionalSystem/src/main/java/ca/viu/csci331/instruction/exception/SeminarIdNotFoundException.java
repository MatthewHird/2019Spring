package ca.viu.csci331.instruction.exception;

public class SeminarIdNotFoundException extends Exception {
    private static final long serialVersionUID = 1L;

    public SeminarIdNotFoundException(String seminarId) {
        super(String.format("Seminar with seminarID \"%s\" not found", seminarId));
    }
}
