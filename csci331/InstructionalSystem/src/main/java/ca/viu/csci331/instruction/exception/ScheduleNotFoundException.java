package ca.viu.csci331.instruction.exception;

import ca.viu.csci331.instruction.model.Schedule;

public class ScheduleNotFoundException extends Exception {

    private static final long serialVersionUID = 1L;

    public ScheduleNotFoundException(Schedule schedule) {
        super(String.format("The following schedule was not found:\n%s", schedule.toString()));
    }
}
