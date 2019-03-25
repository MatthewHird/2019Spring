package ca.viu.csci331.instruction.services.administration;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collections;

import org.junit.Before;
import org.junit.Test;

import ca.viu.csci331.instruction.exception.DuplicateInstructorEmploymentException;
import ca.viu.csci331.instruction.exception.InstructorIdNotFoundException;
import ca.viu.csci331.instruction.model.Instructor;
import ca.viu.csci331.instruction.model.InstructorEmployment;

public class InstructorEmploymentListTests {
    private InstructorEmploymentList instructorEmploymentList;
    
    @Before
    public void populateInstructorEmploymentList() {
        instructorEmploymentList = new InstructorEmploymentList();
        
        try {
            instructorEmploymentList.add("Mitch Richmond", "772748", "Mitch@mail.ca");
            instructorEmploymentList.add("Al Slapper", "223223", "mrslappy@email.com");
            instructorEmploymentList.add("Marky Mark", "123456", "markymark@thefunkybunch.gov");
            instructorEmploymentList.add("Debbie Flarps", "835199", "dflarps@email.com");
        } catch (DuplicateInstructorEmploymentException e) {
            e.printStackTrace();
        }
    }
    
    @Test
    public void testAdd() throws DuplicateInstructorEmploymentException {
        ArrayList<InstructorEmployment> expected = new ArrayList<InstructorEmployment>();
        expected.add(new InstructorEmployment(new Instructor("Mitch Richmond", "772748", "Mitch@mail.ca")));
        expected.add(new InstructorEmployment(new Instructor("Al Slapper", "223223", "mrslappy@email.com")));
        expected.add(new InstructorEmployment(new Instructor("Marky Mark", "123456", "markymark@thefunkybunch.gov")));
        expected.add(new InstructorEmployment(new Instructor("Debbie Flarps", "835199", "dflarps@email.com")));
        expected.add(new InstructorEmployment(new Instructor("The Doug", "444199", "doug@email.com")));
        
        instructorEmploymentList.add("The Doug", "444199", "doug@email.com");
        
        assertTrue(compareInstructorEmploymentArrayList(expected, instructorEmploymentList.getAllInstructorEmployments()));
    }
    
    @Test
    public void testRemove() throws InstructorIdNotFoundException {
        ArrayList<InstructorEmployment> expected = new ArrayList<InstructorEmployment>();
        expected.add(new InstructorEmployment(new Instructor("Mitch Richmond", "772748", "Mitch@mail.ca")));
        expected.add(new InstructorEmployment(new Instructor("Al Slapper", "223223", "mrslappy@email.com")));
        expected.add(new InstructorEmployment(new Instructor("Marky Mark", "123456", "markymark@thefunkybunch.gov")));
        
        instructorEmploymentList.remove("835199");
        assertTrue(compareInstructorEmploymentArrayList(expected, instructorEmploymentList.getAllEmployedInstructorEmployments()));
    }
    
    @Test
    public void testGetAllInstructorEmployments() {
        ArrayList<InstructorEmployment> expected = new ArrayList<InstructorEmployment>();
        expected.add(new InstructorEmployment(new Instructor("Mitch Richmond", "772748", "Mitch@mail.ca")));
        expected.add(new InstructorEmployment(new Instructor("Al Slapper", "223223", "mrslappy@email.com")));
        expected.add(new InstructorEmployment(new Instructor("Marky Mark", "123456", "markymark@thefunkybunch.gov")));
        expected.add(new InstructorEmployment(new Instructor("Debbie Flarps", "835199", "dflarps@email.com")));
        
        assertTrue(compareInstructorEmploymentArrayList(expected, instructorEmploymentList.getAllInstructorEmployments()));
    }
    
    @Test
    public void testGetAllEmployedInstructorEmployments() throws InstructorIdNotFoundException {
        ArrayList<InstructorEmployment> expected = new ArrayList<InstructorEmployment>();
        expected.add(new InstructorEmployment(new Instructor("Mitch Richmond", "772748", "Mitch@mail.ca")));
        expected.add(new InstructorEmployment(new Instructor("Marky Mark", "123456", "markymark@thefunkybunch.gov")));
        expected.add(new InstructorEmployment(new Instructor("Debbie Flarps", "835199", "dflarps@email.com")));
        instructorEmploymentList.searchByInstructorId("223223").setEmploymentStatus("terminated");
        
        assertTrue(compareInstructorEmploymentArrayList(expected, instructorEmploymentList.getAllEmployedInstructorEmployments()));
    }
    
    @Test
    public void testSearchByInstructorName() throws DuplicateInstructorEmploymentException {
        ArrayList<InstructorEmployment> expected = new ArrayList<InstructorEmployment>();
        expected.add(new InstructorEmployment(new Instructor("Mitch Richmond", "772748", "Mitch@mail.ca")));
        
        assertTrue(compareInstructorEmploymentArrayList(expected, instructorEmploymentList.searchByInstructorName("Mitch Richmond")));
        
        expected.add(new InstructorEmployment(new Instructor("Mitch Richmond", "203040", "notMitch@mail.ca")));
        instructorEmploymentList.add("Mitch Richmond", "203040", "notMitch@mail.ca");
        
        assertTrue(compareInstructorEmploymentArrayList(expected, instructorEmploymentList.searchByInstructorName("Mitch Richmond")));
    }
    
    @Test
    public void testSearchByInstructorId() throws InstructorIdNotFoundException {
        InstructorEmployment expected = new InstructorEmployment(new Instructor("Marky Mark", "123456", "markymark@thefunkybunch.gov"));
        
        assertTrue(expected.instructorIdEquals(instructorEmploymentList.searchByInstructorId("123456")));
    }
    
    @Test
    public void testContainsInstructorId() {
        assertTrue(instructorEmploymentList.containsInstructorId("123456"));
        assertFalse(instructorEmploymentList.containsInstructorId("555555"));
    }
    
    private boolean compareInstructorEmploymentArrayList(ArrayList<InstructorEmployment> list1, ArrayList<InstructorEmployment> list2) { 
        if (list1.size() == list2.size()) {
            Collections.sort(list1);
            Collections.sort(list2);
            for (int i = 0; i < list1.size(); i++) {
                if (!list1.get(i).instructorIdEquals(list2.get(i))) {
                    return false;
                }
            }
        } else {
            return false;
        }
        return true;
    }
}
