package ca.viu.csci331.instruction.exception;

public class InvalidAdmissionStatusRuntimeException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    public InvalidAdmissionStatusRuntimeException(String admissionStatus) {
        super("\"" + admissionStatus + "\" is not valid admission status\n"
                + "Must be \"admitted\", \"a\", \"cancelled\", or \"c\"");
    }
}
