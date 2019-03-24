package ca.viu.csci331.instruction.exception;

import ca.viu.csci331.instruction.model.Course;

public class DuplicateCourseNameException extends Exception {
    private static final long serialVersionUID = 1L;
    private String duplicateCourseName;
    
    public DuplicateCourseNameException(Course course) {
        super("Course with name \"" 
                + course.getName() + "\" already exists");
        duplicateCourseName = course.getName();
    }
    
    public DuplicateCourseNameException(String courseName) {
        super("Course with name \"" 
                + courseName + "\" already exists");
        duplicateCourseName = courseName;
    }
    
    public String getDuplicateInstructorId() {
        return duplicateCourseName;
    }
}
