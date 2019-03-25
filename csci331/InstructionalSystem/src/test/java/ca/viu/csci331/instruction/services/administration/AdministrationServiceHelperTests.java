package ca.viu.csci331.instruction.services.administration;

import static org.junit.Assert.assertTrue;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.regex.Pattern;

import org.junit.Test;

import ca.viu.csci331.instruction.model.InstructorEmployment;
import ca.viu.csci331.instruction.model.Instructor;
import ca.viu.csci331.instruction.services.administration.AdministrationServiceHelper;

public class AdministrationServiceHelperTests {
    private ArrayList<InstructorEmployment> instructorEmploymentList;
    
    @Test
    public void testEmploymentTypeFilter() {
        ArrayList<InstructorEmployment> expected1 = new ArrayList<InstructorEmployment>();
        expected1.add(new InstructorEmployment(new Instructor("Mitch Richmond", "772748", "Mitch@mail.ca"), "employed", LocalDate.of(2018, 12, 31)));
        expected1.add(new InstructorEmployment(new Instructor("Al Slapper", "223223", "mrslappy@email.com"), "terminated", LocalDate.of(2019, 1, 2)));
        expected1.add(new InstructorEmployment(new Instructor("Marky Mark", "123456", "markymark@thefunkybunch.gov"), "employed", LocalDate.of(2018, 6, 30)));
        expected1.add(new InstructorEmployment(new Instructor("Debbie Flarps", "835199", "dflarps@email.com"), "terminated", LocalDate.of(2019, 7, 18)));
        ArrayList<InstructorEmployment> expected2 = new ArrayList<InstructorEmployment>();        
        expected2.add(new InstructorEmployment(new Instructor("Mitch Richmond", "772748", "Mitch@mail.ca"), "employed", LocalDate.of(2018, 12, 31)));
        expected2.add(new InstructorEmployment(new Instructor("Marky Mark", "123456", "markymark@thefunkybunch.gov"), "employed", LocalDate.of(2018, 6, 30)));
        ArrayList<InstructorEmployment> expected3 = new ArrayList<InstructorEmployment>();
        expected3.add(new InstructorEmployment(new Instructor("Al Slapper", "223223", "mrslappy@email.com"), "terminated", LocalDate.of(2019, 1, 2)));
        expected3.add(new InstructorEmployment(new Instructor("Debbie Flarps", "835199", "dflarps@email.com"), "terminated", LocalDate.of(2019, 7, 18)));
        
        prepareInstructorEmploymentList();
        assertTrue(compareInstructorEmploymentArrayList(expected1, AdministrationServiceHelper.employmentTypeFilter(instructorEmploymentList, "both")));

        prepareInstructorEmploymentList();
        assertTrue(compareInstructorEmploymentArrayList(expected2, AdministrationServiceHelper.employmentTypeFilter(instructorEmploymentList, "employed")));

        prepareInstructorEmploymentList();
        assertTrue(compareInstructorEmploymentArrayList(expected3, AdministrationServiceHelper.employmentTypeFilter(instructorEmploymentList, "terminated")));
    
    }
    
    @Test
    public void testGetEmployed() {
        ArrayList<InstructorEmployment> expected = new ArrayList<InstructorEmployment>();        
        expected.add(new InstructorEmployment(new Instructor("Mitch Richmond", "772748", "Mitch@mail.ca"), "employed", LocalDate.of(2018, 12, 31)));
        expected.add(new InstructorEmployment(new Instructor("Marky Mark", "123456", "markymark@thefunkybunch.gov"), "employed", LocalDate.of(2018, 6, 30)));
        
        prepareInstructorEmploymentList();
        assertTrue(compareInstructorEmploymentArrayList(expected, AdministrationServiceHelper.getEmployed(instructorEmploymentList)));
    
    }
    
    @Test
    public void testGetTerminated() {
        ArrayList<InstructorEmployment> expected = new ArrayList<InstructorEmployment>();
        expected.add(new InstructorEmployment(new Instructor("Al Slapper", "223223", "mrslappy@email.com"), "terminated", LocalDate.of(2019, 1, 2)));
        expected.add(new InstructorEmployment(new Instructor("Debbie Flarps", "835199", "dflarps@email.com"), "terminated", LocalDate.of(2019, 7, 18)));

        prepareInstructorEmploymentList();
        assertTrue(compareInstructorEmploymentArrayList(expected, AdministrationServiceHelper.getTerminated(instructorEmploymentList)));
    
    }
    
    @Test
    public void testGenerateInstructorId() {
        for (int i = 0; i < 200; i++) {
          assertTrue(Pattern.matches("^[1-9][0-9]{5}$", AdministrationServiceHelper.generateInstructorId()));
      }
    }
    
    @Test
    public void testGenerateScheduleId() {
        for (int i = 0; i < 200; i++) {
          assertTrue(Pattern.matches("^[1-9][0-9]{8}$", AdministrationServiceHelper.generateScheduleId()));
      }
    }
    
    private void prepareInstructorEmploymentList() {
        instructorEmploymentList = new ArrayList<InstructorEmployment>();

        instructorEmploymentList.add(new InstructorEmployment(new Instructor("Mitch Richmond", "772748", "Mitch@mail.ca"), "employed", LocalDate.of(2018, 12, 31)));
        instructorEmploymentList.add(new InstructorEmployment(new Instructor("Al Slapper", "223223", "mrslappy@email.com"), "terminated", LocalDate.of(2019, 1, 2)));
        instructorEmploymentList.add(new InstructorEmployment(new Instructor("Marky Mark", "123456", "markymark@thefunkybunch.gov"), "employed", LocalDate.of(2018, 6, 30)));
        instructorEmploymentList.add(new InstructorEmployment(new Instructor("Debbie Flarps", "835199", "dflarps@email.com"), "terminated", LocalDate.of(2019, 7, 18)));
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
