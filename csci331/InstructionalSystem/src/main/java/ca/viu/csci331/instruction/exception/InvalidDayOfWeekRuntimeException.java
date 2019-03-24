package ca.viu.csci331.instruction.exception;

public class InvalidDayOfWeekRuntimeException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    public InvalidDayOfWeekRuntimeException(String invalidDay) {
        super(String.format("Invalid day of week: \"%s\"\nValid day of week "
                + "values are case insensitive as follows:\n    \"sun\", "
                + "\"sunday\", \"mon\", \"monday\", \"tue\", \"tues\", "
                + "\"tuesday\", \"wed\", \"wednesday\",\n    \"thu\", "
                + "\"thur\", \"thurs\", \"thursday\", \"fri\",\"friday\", "
                + "\"sat\", or \"saturday\"", invalidDay));
    }
}
