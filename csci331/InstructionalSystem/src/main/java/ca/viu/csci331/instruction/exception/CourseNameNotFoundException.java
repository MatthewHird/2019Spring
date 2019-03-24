package ca.viu.csci331.instruction.exception;

public class CourseNameNotFoundException extends Exception {

    private static final long serialVersionUID = 1L;

    public CourseNameNotFoundException(String courseName) {
        super("Course with name " + courseName + " not found");
    }
}
