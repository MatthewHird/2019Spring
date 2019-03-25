package ca.viu.csci331.instruction.services.administration;

import java.util.ArrayList;
import java.util.List;

import ca.viu.csci331.instruction.exception.CourseNameNotFoundException;
import ca.viu.csci331.instruction.exception.CourseNumberNotFoundException;
import ca.viu.csci331.instruction.exception.DuplicateCourseNameException;
import ca.viu.csci331.instruction.exception.DuplicateCourseNumberException;
import ca.viu.csci331.instruction.model.Course;

/**
 * Business class used to manage a list of <Course> ocjects (courses available to register for) at a university.
 * @author Matthew Hird
 * @date Mar. 21, 2019
 */
public class OfferedCourseList {
    private List<Course> courseList;
    
    public OfferedCourseList() {
        courseList = new ArrayList<Course>();
    }
    
    
    public int getCourseCount() {
        return courseList.size();
    }
    
    
    public void add(String courseName, String courseNumber, double courseCredit, String courseDescription) 
            throws DuplicateCourseNameException, DuplicateCourseNumberException {
        if (containsCourseName(courseName)) {
            throw new DuplicateCourseNameException(courseName);
        } else if (containsCourseNumber(courseNumber)) {
            throw new DuplicateCourseNumberException(courseNumber);
        }
        
        courseList.add(new Course(courseName, courseNumber, courseCredit, courseDescription));
    };

    
    public Course remove(String cancelledCourseNumber) throws CourseNumberNotFoundException {
        int removeIndex = -1;
        for (int i = 0; i < courseList.size(); i++) {
            if (courseList.get(i).getCourseNumber().equals(cancelledCourseNumber)) {
                removeIndex = i;
                break;
            }
        }
        if (removeIndex == -1) {
            throw new CourseNumberNotFoundException(cancelledCourseNumber);
        }
        return courseList.remove(removeIndex);
    }
    
    public Course searchByName(String courseName) throws CourseNameNotFoundException {
        for (Course course : courseList) {
            if (course.getName().equals(courseName)) {
                return course;
            }
        }
        throw new CourseNameNotFoundException(courseName);
    }
    
    public Course searchByNumber(String courseNumber) throws CourseNumberNotFoundException {
        for (Course course : courseList) {
            if (course.getCourseNumber().equals(courseNumber)) {
                return course;
            }
        }
        throw new CourseNumberNotFoundException(courseNumber);
    }
    
    public ArrayList<Course> getAllOfferedCourses() {
        ArrayList<Course> allOfferedCourses = new ArrayList<Course>();
        for (Course course : courseList) {
            allOfferedCourses.add(course);
        }
        return allOfferedCourses;
    }
    
    public boolean containsCourseName(String courseName) {
        for (Course course : courseList) {
            if (course.getName().equals(courseName)) {
                return true;
            }
        }
        return false;
    }
    
    public boolean containsCourseNumber(String courseNumber) {
        for (Course course : courseList) {
            if (course.getCourseNumber().equals(courseNumber)) {
                return true;
            }
        }
        return false;
    }
    
//    public String allToString() {
//        String asString = "";
//        for (Course course : courseList) {
//            asString += course.toString();
//        }
//        return asString;
//    }
}
