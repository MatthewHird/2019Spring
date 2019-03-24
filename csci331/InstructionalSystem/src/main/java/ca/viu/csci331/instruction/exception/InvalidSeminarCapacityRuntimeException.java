package ca.viu.csci331.instruction.exception;

public class InvalidSeminarCapacityRuntimeException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    public InvalidSeminarCapacityRuntimeException(int invalidCapacity) {
        super(String.format("Invalid seminar capacity: \"%d\"\n"
                + "    Seminar capacity must be >= 0", invalidCapacity));
    }
}
