package ca.viu.csci331.instruction.exception;

public class CourseNumberNotFoundException extends Exception {

    private static final long serialVersionUID = 1L;

    public CourseNumberNotFoundException(String courseNumber) {
        super("Course with course number " + courseNumber + " not found");
    }
}
