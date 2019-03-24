package ca.viu.csci331.instruction.exception;

import ca.viu.csci331.instruction.model.Seminar;

public class DuplicateSeminarException extends Exception {
    private static final long serialVersionUID = 1L;
    private String duplicateSeminarId;
    
    public DuplicateSeminarException(Seminar seminar) {
        super(String.format("Seminar with seminarID \"%s\" already exists", seminar.getSeminarId()));
        duplicateSeminarId = seminar.getSeminarId();
    }
    
    public DuplicateSeminarException(String seminarId) {
        super(String.format("Seminar with seminarID \"%s\" already exists", seminarId));
        duplicateSeminarId = seminarId;
    }
    
    public String getDuplicateStudentId() {
        return duplicateSeminarId;
    }
}
