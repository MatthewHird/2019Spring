package ca.viu.csci331.instruction.exception;

import ca.viu.csci331.instruction.model.Course;

public class CourseNotFoundException extends Exception {

    private static final long serialVersionUID = 1L;

    public CourseNotFoundException(Course course) {
        super("Course with course number " + course.getCourseNumber() + " not found");
    }
}
