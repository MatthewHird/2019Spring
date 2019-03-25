package ca.viu.csci331.instruction.services.administration;

import java.util.ArrayList;
import java.util.List;

import ca.viu.csci331.instruction.exception.CourseNameNotFoundException;
import ca.viu.csci331.instruction.exception.CourseNumberNotFoundException;
import ca.viu.csci331.instruction.exception.DuplicateCourseNameException;
import ca.viu.csci331.instruction.exception.DuplicateCourseNumberException;
import ca.viu.csci331.instruction.model.Course;

/**
 * Contains and manages Course objects in a list.
 * 
 * @author Matthew Hird
 * @date Mar. 25, 2019
 */
public class OfferedCourseList {
    private List<Course> courseList;
    
    public OfferedCourseList() {
        courseList = new ArrayList<Course>();
    }
    
    /**
     * @return  Number of Course objects currently in OfferedCourseList.
     */
    public int getCourseCount() {
        return courseList.size();
    }
    
    /**
     * @param courseName        Name of added Course.
     * @param courseNumber      ID of the added Course.
     * @param courseCredit      Credit value of the added Course.
     * @param courseDescription A description of the added course.
     * @throws DuplicateCourseNameException     If Course with name courseName
     *                                          is already in OfferedCourseList.
     * @throws DuplicateCourseNumberException   If course with IS courseNumber
     *                                          is already in OfferedCourseList.
     */
    public void add(String courseName, String courseNumber, double courseCredit, String courseDescription) 
            throws DuplicateCourseNameException, DuplicateCourseNumberException {
        if (containsCourseName(courseName)) {
            throw new DuplicateCourseNameException(courseName);
        } else if (containsCourseNumber(courseNumber)) {
            throw new DuplicateCourseNumberException(courseNumber);
        }
        
        courseList.add(new Course(courseName, courseNumber, courseCredit, courseDescription));
    };
    
    /**
     * @param cancelledCourseNumber     ID of the Course to be cancelled.
     * @return  Course that was cancelled.
     * @throws CourseNumberNotFoundException    If course with ID 
     *              cancelledCourseNumber is not in OfferedCourseList.
     */
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
    
    /**
     * Find Course based on specified name.
     * 
     * @param courseName    Name of Course.
     * @return  Course with name courseName.
     * @throws CourseNameNotFoundException  If Course with name courseName is
     *                                      not in OfferedCourseList.
     */
    public Course searchByName(String courseName) throws CourseNameNotFoundException {
        for (Course course : courseList) {
            if (course.getName().equals(courseName)) {
                return course;
            }
        }
        throw new CourseNameNotFoundException(courseName);
    }
    
    /**
     * Find Course based on course ID.
     * 
     * @param courseNumber  ID of Course.
     * @return  Course with ID courseNumber.
     * @throws CourseNumberNotFoundException    If Course with ID courseNumber
     *                                          is not in OfferedCourseList.
     */
    public Course searchByNumber(String courseNumber) throws CourseNumberNotFoundException {
        for (Course course : courseList) {
            if (course.getCourseNumber().equals(courseNumber)) {
                return course;
            }
        }
        throw new CourseNumberNotFoundException(courseNumber);
    }
    
    /**
     * @return  List of all Courses in OfferedCourseList.
     */
    public ArrayList<Course> getAllOfferedCourses() {
        ArrayList<Course> allOfferedCourses = new ArrayList<Course>();
        for (Course course : courseList) {
            allOfferedCourses.add(course);
        }
        return allOfferedCourses;
    }
    
    /**
     * Checks for exiestence of a Course with specified name.
     * 
     * @param courseName    Name of Course.
     * @return  true if a Course with name courseName is in OfferedCoureList.
     *          Otherwise, false.
     */
    public boolean containsCourseName(String courseName) {
        for (Course course : courseList) {
            if (course.getName().equals(courseName)) {
                return true;
            }
        }
        return false;
    }
    
    /**
     * Checks for existence of a Course with specified ID.
     * 
     * @param courseNumber    Name of Course.
     * @return  true if a Course with ID courseNumber is in OfferedCoureList.
     *          Otherwise, false.
     */
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
