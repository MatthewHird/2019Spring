package ca.viu.csci331.instruction.model;

import java.util.ArrayList;
import java.util.List;

public class OfferedCourses {
    private List<Course> courses;
    private int capacity;
    private int courseCount;
    
    public OfferedCourses(int capacity) {
        this.capacity = capacity;
        courseCount = 0;
        courses = new ArrayList<Course>();
    }
    
    public void offer(Course offeredCourse) {
        if (courseCount < capacity) {
            courses.add(offeredCourse);
            courseCount++;
        } else {
            System.out.print("Could not offer course: Offered courses list at capacity\n\n");
        }
    }
    
    public void cancel(Course cancelledCourse) {
        courses.removeIf(s -> (s.getCourseNumber().equals(cancelledCourse.getCourseNumber())));
        courseCount = courses.size();
    }
    
    public void showAll() {
        System.out.print("\n~~~~~~~~~~~~~~~~~~~~\nList of All Courses\n~~~~~~~~~~~~~~~~~~~~\n\n");
        for (Course course: courses) {
            course.show();
        }
        System.out.print("~~~~~~~~~~~~~~~~~~~~\n\n");
    }
    
    public Course searchByName(String courseName) {
        return new Course("COURSENOTFOUND", "COURSENOTFOUND", 0, "");
    }
    
    public Course searchByNumber(String courseNumber) {
        return new Course("COURSENOTFOUND", "COURSENOTFOUND", 0, "");
    }
}
