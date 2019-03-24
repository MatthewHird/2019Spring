package ca.viu.csci331.instruction.exception;

import ca.viu.csci331.instruction.model.Course;

public class DuplicateCourseNumberException extends Exception {
    private static final long serialVersionUID = 1L;
    private String duplicateCourseNumber;
    
    public DuplicateCourseNumberException(Course course) {
        super("Course with course number \"" 
                + course.getCourseNumber() + "\" already exists");
        duplicateCourseNumber = course.getCourseNumber();
    }
    
    public DuplicateCourseNumberException(String courseNumber) {
        super("Course with course number \"" 
                + courseNumber + "\" already exists");
        duplicateCourseNumber = courseNumber;
    }
    
    public String getDuplicateInstructorId() {
        return duplicateCourseNumber;
    }
}
