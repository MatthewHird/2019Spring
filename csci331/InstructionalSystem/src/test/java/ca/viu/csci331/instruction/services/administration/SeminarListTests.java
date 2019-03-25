package ca.viu.csci331.instruction.services.administration;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collections;

import org.junit.Before;
import org.junit.Test;

import ca.viu.csci331.instruction.exception.DuplicateSeminarException;
import ca.viu.csci331.instruction.exception.SeminarIdNotFoundException;
import ca.viu.csci331.instruction.model.Course;
import ca.viu.csci331.instruction.model.Instructor;
import ca.viu.csci331.instruction.model.Seminar;

public class SeminarListTests {
    private SeminarList seminarList;
    
    @Before
    public void prepareSeminarList() {
        seminarList = new SeminarList();
        
        try {
            seminarList.add("SM01", new Course("Course One", "C001", 3, "Words"), 20, new Instructor("Name 1", "111111", "a@b.com"));
            seminarList.add("SM02", new Course("Course One", "C001", 3, "Words"), 20, new Instructor("Name 1", "111111", "a@b.com"));
            seminarList.add("SM03", new Course("Course One", "C001", 3, "Words"), 20, new Instructor("Name 1", "111111", "a@b.com"));
        } catch (DuplicateSeminarException e) {
            e.printStackTrace();
        }
    }
    
    @Test
    public void testAdd() throws DuplicateSeminarException {
        ArrayList<Seminar> expected = new ArrayList<Seminar>();
        expected.add(new Seminar("SM01", new Course("Course One", "C001", 3, "Words"), 20, new Instructor("Name 1", "111111", "a@b.com")));
        expected.add(new Seminar("SM02", new Course("Course One", "C001", 3, "Words"), 20, new Instructor("Name 1", "111111", "a@b.com")));
        expected.add(new Seminar("SM03", new Course("Course One", "C001", 3, "Words"), 20, new Instructor("Name 1", "111111", "a@b.com")));
        expected.add(new Seminar("SM04", new Course("Course One", "C001", 3, "Words"), 20, new Instructor("Name 1", "111111", "a@b.com")));
        
        seminarList.add("SM04", new Course("Course One", "C001", 3, "Words"), 20, new Instructor("Name 1", "111111", "a@b.com"));
        assertTrue(compareSeminarArrayList(expected, seminarList.getAllSeminars()));
    }
    
    @Test
    public void testRemove() throws SeminarIdNotFoundException {
        ArrayList<Seminar> expected = new ArrayList<Seminar>();
        expected.add(new Seminar("SM01", new Course("Course One", "C001", 3, "Words"), 20, new Instructor("Name 1", "111111", "a@b.com")));
        expected.add(new Seminar("SM02", new Course("Course One", "C001", 3, "Words"), 20, new Instructor("Name 1", "111111", "a@b.com")));
        
        seminarList.remove("SM03");
        assertTrue(compareSeminarArrayList(expected, seminarList.getAllSeminars()));
    }
    
    @Test
    public void testSearchBySeminarId() throws SeminarIdNotFoundException {
        Seminar expected = new Seminar("SM03", new Course("Course One", "C001", 3, "Words"), 20, new Instructor("Name 1", "111111", "a@b.com"));
        assertTrue(expected.seminarIdEquals(seminarList.searchBySeminarId("SM03")));
    }
    
    @Test
    public void testContainsSeminarId() {
        assertTrue(seminarList.containsSeminarId("SM02"));
        assertFalse(seminarList.containsSeminarId("SM11"));
    }
    
    private boolean compareSeminarArrayList(ArrayList<Seminar> list1, ArrayList<Seminar> list2) { 
        if (list1.size() == list2.size()) {
            Collections.sort(list1);
            Collections.sort(list2);
            for (int i = 0; i < list1.size(); i++) {
                if (!list1.get(i).seminarIdEquals(list2.get(i))) {
                    return false;
                }
            }
        } else {
            return false;
        }
        return true;
    }
}
