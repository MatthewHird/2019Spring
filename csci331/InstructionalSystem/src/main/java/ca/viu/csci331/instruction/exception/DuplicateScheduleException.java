package ca.viu.csci331.instruction.exception;

import ca.viu.csci331.instruction.model.Schedule;

public class DuplicateScheduleException extends Exception {
    private static final long serialVersionUID = 1L;
    
    public DuplicateScheduleException() {
        super();
    }
    
    public DuplicateScheduleException(Schedule schedule) {
        super(String.format("The following schedule overlaps with another schedule:\n%s", schedule.toString()));
    }
}
