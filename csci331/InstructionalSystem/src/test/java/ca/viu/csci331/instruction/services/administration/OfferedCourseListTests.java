package ca.viu.csci331.instruction.services.administration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collections;

import org.junit.Before;
import org.junit.Test;

import ca.viu.csci331.instruction.exception.CourseNameNotFoundException;
import ca.viu.csci331.instruction.exception.CourseNumberNotFoundException;
import ca.viu.csci331.instruction.exception.DuplicateCourseNameException;
import ca.viu.csci331.instruction.exception.DuplicateCourseNumberException;
import ca.viu.csci331.instruction.model.Course;

public class OfferedCourseListTests {
    private OfferedCourseList offeredCourseList;
    
    @Before
    public void populateOfferedCourseList() {
        offeredCourseList = new OfferedCourseList();

        try {
            offeredCourseList.add("Course One", "C001", 3.0, "Some words.");
            offeredCourseList.add("Course Two", "C002", 2.0, "Some");
            offeredCourseList.add("Course Three", "C003", 1.0, "Stuff");
        } catch (DuplicateCourseNameException | DuplicateCourseNumberException e) {
            e.printStackTrace();
        }

    }
    
    @Test
    public void testAdd() throws DuplicateCourseNameException, DuplicateCourseNumberException {
        ArrayList<Course> expected = new ArrayList<Course>();
        expected.add(new Course("Course One", "C001", 3.0, "Some words."));
        expected.add(new Course("Course Two", "C002", 2.0, "Some"));
        expected.add(new Course("Course Three", "C003", 1.0, "Stuff"));
        expected.add(new Course("Course Four", "C004", 1.0, ""));
        
        offeredCourseList.add("Course Four", "C004", 1.0, "");
        assertTrue(compareCourseArrayList(expected, offeredCourseList.getAllOfferedCourses()));
    }
    
    @Test
    public void testRemove() throws CourseNumberNotFoundException {
        ArrayList<Course> expected = new ArrayList<Course>();
        expected.add(new Course("Course One", "C001", 3.0, "Some words."));
        expected.add(new Course("Course Two", "C002", 2.0, "Some"));
        
        offeredCourseList.remove("C003");
        assertTrue(compareCourseArrayList(expected, offeredCourseList.getAllOfferedCourses()));
    }
    
    @Test
    public void testSearchByName() throws CourseNameNotFoundException {
        Course expected = new Course("Course Two", "C002", 2.0, "Some");
        assertEquals(expected.getCourseNumber(), offeredCourseList.searchByName("Course Two").getCourseNumber());
    }
    
    @Test
    public void testSearchByNumber() throws CourseNumberNotFoundException {
        Course expected = new Course("Course Three", "C003", 1.0, "Stuff");
        assertEquals(expected.getCourseNumber(), offeredCourseList.searchByNumber("C003").getCourseNumber());
    }
    
    @Test
    public void testGetAllOfferedCourses() {
        ArrayList<Course> expected = new ArrayList<Course>();
        expected.add(new Course("Course One", "C001", 3.0, "Some words."));
        expected.add(new Course("Course Two", "C002", 2.0, "Some"));
        expected.add(new Course("Course Three", "C003", 1.0, "Stuff"));
        
        assertTrue(compareCourseArrayList(expected, offeredCourseList.getAllOfferedCourses()));
    }
    
    @Test
    public void testContainsCourseName() {
        assertTrue(offeredCourseList.containsCourseName("Course One"));
        assertFalse(offeredCourseList.containsCourseName("Course Four"));
    }
    
    @Test
    public void testContainsCourseNumber() {
        assertTrue(offeredCourseList.containsCourseNumber("C002"));
        assertFalse(offeredCourseList.containsCourseNumber("C111"));
    }
    
    private boolean compareCourseArrayList(ArrayList<Course> list1, ArrayList<Course> list2) { 
        if (list1.size() == list2.size()) {
            Collections.sort(list1);
            Collections.sort(list2);
            for (int i = 0; i < list1.size(); i++) {
                if (!list1.get(i).getCourseNumber().equals(list2.get(i).getCourseNumber())) {
                    return false;
                }
            }
        } else {
            return false;
        }
        return true;
    }
}
