package ca.viu.csci331.instruction.exception;

public class InvalidRoomCapacityRuntimeException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    public InvalidRoomCapacityRuntimeException(int invalidCapacity) {
        super(String.format("Invalid room capacity: \"%d\"\n"
                + "    Room capacity must be >= 0", invalidCapacity));
    }
}
