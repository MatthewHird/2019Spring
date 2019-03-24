package ca.viu.csci331.instruction.exception;

public class ScheduleIdNotFoundException extends Exception {

    private static final long serialVersionUID = 1L;

    public ScheduleIdNotFoundException(String scheduleId) {
        super(String.format("Schedule with schedule ID \"%s\" not found:\n", scheduleId));
    }
}
