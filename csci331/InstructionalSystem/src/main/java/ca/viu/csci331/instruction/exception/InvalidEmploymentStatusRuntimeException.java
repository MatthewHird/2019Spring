package ca.viu.csci331.instruction.exception;

public class InvalidEmploymentStatusRuntimeException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    public InvalidEmploymentStatusRuntimeException(String employmentStatus) {
        super("\"" + employmentStatus + "\" is not valid employment status\n"
                + "Must be \"employed\", \"a\", \"terminated\", or \"c\"");
    }
}
