package ca.viu.csci331.instruction.exception;

public class SeminarRoomCapacityConflictException extends Exception {
    private static final long serialVersionUID = 1L;

    public SeminarRoomCapacityConflictException() {
        super(String.format("SeminarRoomCapacityConflict: Seminar cannot have a capacity greater than the capacity of scheduled room"));
    }
}
