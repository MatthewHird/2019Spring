package ca.viu.csci331.instruction.exception;

public class InvalidDurationRuntimeException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    public InvalidDurationRuntimeException(int invalidDuration) {
        super(String.format("Invalid duration: \"%d\"\n"
                + "    Duration must be > 0 (minutes)", invalidDuration));
    }
}
