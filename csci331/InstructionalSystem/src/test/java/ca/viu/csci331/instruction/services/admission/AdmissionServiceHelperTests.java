package ca.viu.csci331.instruction.services.admission;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.regex.Pattern;

import org.junit.Test;

import ca.viu.csci331.instruction.model.Student;
import ca.viu.csci331.instruction.model.StudentAdmission;

public class AdmissionServiceHelperTests {
    private ArrayList<StudentAdmission> studentAdmissionList;
    
    
    @Test
    public void testAdmissionTypeFilter() {
        ArrayList<StudentAdmission> expected1 = new ArrayList<StudentAdmission>();
        expected1.add(new StudentAdmission(new Student("Mitch Richmond", "772748212", "Mitch@mail.ca"), "admitted", LocalDate.of(2018, 12, 31)));
        expected1.add(new StudentAdmission(new Student("Al Slapper", "223223223", "mrslappy@email.com"), "cancelled", LocalDate.of(2019, 1, 2)));
        expected1.add(new StudentAdmission(new Student("Marky Mark", "123456789", "markymark@thefunkybunch.gov"), "admitted", LocalDate.of(2018, 6, 30)));
        expected1.add(new StudentAdmission(new Student("Debbie Flarps", "835199928", "dflarps@email.com"), "cancelled", LocalDate.of(2019, 7, 18)));
        ArrayList<StudentAdmission> expected2 = new ArrayList<StudentAdmission>();        
        expected2.add(new StudentAdmission(new Student("Mitch Richmond", "772748212", "Mitch@mail.ca"), "admitted", LocalDate.of(2018, 12, 31)));
        expected2.add(new StudentAdmission(new Student("Marky Mark", "123456789", "markymark@thefunkybunch.gov"), "admitted", LocalDate.of(2018, 6, 30)));
        ArrayList<StudentAdmission> expected3 = new ArrayList<StudentAdmission>();
        expected3.add(new StudentAdmission(new Student("Al Slapper", "223223223", "mrslappy@email.com"), "cancelled", LocalDate.of(2019, 1, 2)));
        expected3.add(new StudentAdmission(new Student("Debbie Flarps", "835199928", "dflarps@email.com"), "cancelled", LocalDate.of(2019, 7, 18)));
        
        prepareStudentAdmissionList();
        assertTrue(compareStudentAdmissionArrayList(expected1, AdmissionServiceHelper.admissionTypeFilter(studentAdmissionList, "both")));

        prepareStudentAdmissionList();
        assertTrue(compareStudentAdmissionArrayList(expected2, AdmissionServiceHelper.admissionTypeFilter(studentAdmissionList, "admitted")));

        prepareStudentAdmissionList();
        assertTrue(compareStudentAdmissionArrayList(expected3, AdmissionServiceHelper.admissionTypeFilter(studentAdmissionList, "cancelled")));
    }
    
    
    @Test
    public void testGetAdmitted() {
        ArrayList<StudentAdmission> expected = new ArrayList<StudentAdmission>();        
        expected.add(new StudentAdmission(new Student("Mitch Richmond", "772748212", "Mitch@mail.ca"), "admitted", LocalDate.of(2018, 12, 31)));
        expected.add(new StudentAdmission(new Student("Marky Mark", "123456789", "markymark@thefunkybunch.gov"), "admitted", LocalDate.of(2018, 6, 30)));
        
        prepareStudentAdmissionList();
        assertTrue(compareStudentAdmissionArrayList(expected, AdmissionServiceHelper.getAdmitted(studentAdmissionList)));
    }
    
    
    @Test
    public void testGetCancelled() {
        ArrayList<StudentAdmission> expected = new ArrayList<StudentAdmission>();
        expected.add(new StudentAdmission(new Student("Al Slapper", "223223223", "mrslappy@email.com"), "cancelled", LocalDate.of(2019, 1, 2)));
        expected.add(new StudentAdmission(new Student("Debbie Flarps", "835199928", "dflarps@email.com"), "cancelled", LocalDate.of(2019, 7, 18)));

        prepareStudentAdmissionList();
        assertTrue(compareStudentAdmissionArrayList(expected, AdmissionServiceHelper.getCancelled(studentAdmissionList)));
    }
    
    
    @Test
    public void testGenerateStudentId() {
        for (int i = 0; i < 200; i++) {
            assertTrue(Pattern.matches("^[1-9][0-9]{8}$", AdmissionServiceHelper.generateStudentId()));
        }
    }
    
    
    @Test
    public void testGenerateStudentReport() {
        prepareStudentAdmissionList();
        String expected = "Student Name: Mitch Richmond\nStudent ID: 772748212\nEmail: Mitch@mail.ca\nAdmission Status: admitted\nAdmission Date: 2018-12-31"
                + "\n\nStudent Name: Al Slapper\nStudent ID: 223223223\nEmail: mrslappy@email.com\nAdmission Status: cancelled\nAdmission Date: 2019-01-02\n\n"
                + "Student Name: Marky Mark\nStudent ID: 123456789\nEmail: markymark@thefunkybunch.gov\nAdmission Status: admitted\nAdmission Date: 2018-06-30"
                + "\n\nStudent Name: Debbie Flarps\nStudent ID: 835199928\nEmail: dflarps@email.com\nAdmission Status: cancelled\nAdmission Date: 2019-07-18\n\n";
        
        assertEquals(expected, AdmissionServiceHelper.generateStudentReport(studentAdmissionList));
    }
    
    
    private void prepareStudentAdmissionList() {
        studentAdmissionList = new ArrayList<StudentAdmission>();

        studentAdmissionList.add(new StudentAdmission(new Student("Mitch Richmond", "772748212", "Mitch@mail.ca"), "admitted", LocalDate.of(2018, 12, 31)));
        studentAdmissionList.add(new StudentAdmission(new Student("Al Slapper", "223223223", "mrslappy@email.com"), "cancelled", LocalDate.of(2019, 1, 2)));
        studentAdmissionList.add(new StudentAdmission(new Student("Marky Mark", "123456789", "markymark@thefunkybunch.gov"), "admitted", LocalDate.of(2018, 6, 30)));
        studentAdmissionList.add(new StudentAdmission(new Student("Debbie Flarps", "835199928", "dflarps@email.com"), "cancelled", LocalDate.of(2019, 7, 18)));
    }
    
    
    private boolean compareStudentAdmissionArrayList(ArrayList<StudentAdmission> list1, ArrayList<StudentAdmission> list2) { 
        if (list1.size() == list2.size()) {
            Collections.sort(list1);
            Collections.sort(list2);
            for (int i = 0; i < list1.size(); i++) {
                if (!list1.get(i).studentIdEquals(list2.get(i))) {
                    return false;
                }
            }
        } else {
            return false;
        }
        
        return true;
    }
}
