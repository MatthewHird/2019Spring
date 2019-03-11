package ca.viu.csci331.instruction.services.admission;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import ca.viu.csci331.instruction.exception.DuplicateStudentAdmissionException;
import ca.viu.csci331.instruction.exception.StudentIdNotFoundException;
import ca.viu.csci331.instruction.model.Student;
import ca.viu.csci331.instruction.model.StudentAdmission;


public class StudentAdmissionListTests {
    private StudentAdmissionList studentAdmissionList; 
    
    @Before
    public void populateStudentAdmissionList() {
        studentAdmissionList = new StudentAdmissionList();
        try {
            studentAdmissionList.add(new StudentAdmission(new Student("Jack Maxim", "289403729", "jackMax@mymail.org"), "admitted", LocalDate.of(2019, 1, 1)));
            studentAdmissionList.add(new StudentAdmission(new Student("Jane Dome", "903894392", "janeD@yourmail.org"), "admitted", LocalDate.of(2019, 1, 1)));
            studentAdmissionList.add(new StudentAdmission(new Student("Mitch Richmond", "772748212", "Mitch@mail.ca"), "admitted", LocalDate.of(2018, 12, 31)));
            studentAdmissionList.add(new StudentAdmission(new Student("Al Slapper", "223223223", "mrslappy@email.com"), "cancelled", LocalDate.of(2019, 1, 2)));
            studentAdmissionList.add(new StudentAdmission(new Student("Marky Mark", "123456789", "markymark@thefunkybunch.gov"), "admitted", LocalDate.of(2018, 6, 30)));
            studentAdmissionList.add(new StudentAdmission(new Student("Debbie Flarps", "835199928", "dflarps@email.com"), "cancelled", LocalDate.of(2019, 7, 18)));
            studentAdmissionList.add(new StudentAdmission(new Student("Jane Dome", "656463626", "email@mymail.mail"), "admitted", LocalDate.of(2019, 7, 19)));
            studentAdmissionList.add(new StudentAdmission(new Student("Quinn Terrot", "385489739", "qtpi@cmail.com"), "cancelled", LocalDate.of(2017, 6, 21)));
            studentAdmissionList.add(new StudentAdmission(new Student("Doug", "876321053", "thedougie@thefunkybunch.gov"), "admitted", LocalDate.of(2017, 6, 21)));
            studentAdmissionList.add(new StudentAdmission(new Student("Jack Foxx", "222222222", "Foxxie5055@cmail.com"), "cancelled", LocalDate.of(2010, 2, 21)));
        } catch (DuplicateStudentAdmissionException e) {
            System.out.println(e.getMessage());
            e.printStackTrace();
        }
    }
    
    
    @Rule
    public ExpectedException expectedException = ExpectedException.none();
    
    
    @Test
    public void testGetStudentCount() {
        assertEquals(10, studentAdmissionList.getStudentCount());
    }
    
    
    @Test
    public void testAllToString() {
        String expected = "Jack Maxim\n289403729\njackMax@mymail.org\nadmitted\n2019-01-01\nJane Dome\n903894392\njaneD@yourmail.org\nadmitted\n2019-01-01\n"
                + "Mitch Richmond\n772748212\nMitch@mail.ca\nadmitted\n2018-12-31\nAl Slapper\n223223223\nmrslappy@email.com\ncancelled\n2019-01-02\n"
                + "Marky Mark\n123456789\nmarkymark@thefunkybunch.gov\nadmitted\n2018-06-30\nDebbie Flarps\n835199928\ndflarps@email.com\ncancelled\n"
                + "2019-07-18\nJane Dome\n656463626\nemail@mymail.mail\nadmitted\n2019-07-19\nQuinn Terrot\n385489739\nqtpi@cmail.com\ncancelled\n"
                + "2017-06-21\nDoug\n876321053\nthedougie@thefunkybunch.gov\nadmitted\n2017-06-21\nJack Foxx\n222222222\nFoxxie5055@cmail.com\n"
                + "cancelled\n2010-02-21\n";
        assertEquals(expected, studentAdmissionList.allToString());
    }
    
    
    @Test
    public void testAddParamStudentAdmission() {
        String expected = "Jack Maxim\n289403729\njackMax@mymail.org\nadmitted\n2019-01-01\nJane Dome\n903894392\njaneD@yourmail.org\nadmitted\n2019-01-01\n"
                + "Mitch Richmond\n772748212\nMitch@mail.ca\nadmitted\n2018-12-31\nAl Slapper\n223223223\nmrslappy@email.com\ncancelled\n2019-01-02\n"
                + "Marky Mark\n123456789\nmarkymark@thefunkybunch.gov\nadmitted\n2018-06-30\nDebbie Flarps\n835199928\ndflarps@email.com\ncancelled\n"
                + "2019-07-18\nJane Dome\n656463626\nemail@mymail.mail\nadmitted\n2019-07-19\nQuinn Terrot\n385489739\nqtpi@cmail.com\ncancelled\n"
                + "2017-06-21\nDoug\n876321053\nthedougie@thefunkybunch.gov\nadmitted\n2017-06-21\nJack Foxx\n222222222\nFoxxie5055@cmail.com\n"
                + "cancelled\n2010-02-21\nBobbie Jean\n876333333\nbj@mymail.org\nadmitted\n2010-06-21\n";
        try {
            studentAdmissionList.add(new StudentAdmission(new Student("Bobbie Jean", "876333333", "bj@mymail.org"), "admitted", LocalDate.of(2010, 6, 21)));
        } catch (DuplicateStudentAdmissionException e) {
            System.out.println(e.getMessage());
            e.printStackTrace();
        }
        assertEquals(expected, studentAdmissionList.allToString());
    }
    
    
    @Test
    public void testAddParamsStringStringStringStringLocalDate() {
        String expected = "Jack Maxim\n289403729\njackMax@mymail.org\nadmitted\n2019-01-01\nJane Dome\n903894392\njaneD@yourmail.org\nadmitted\n2019-01-01\n"
                + "Mitch Richmond\n772748212\nMitch@mail.ca\nadmitted\n2018-12-31\nAl Slapper\n223223223\nmrslappy@email.com\ncancelled\n2019-01-02\n"
                + "Marky Mark\n123456789\nmarkymark@thefunkybunch.gov\nadmitted\n2018-06-30\nDebbie Flarps\n835199928\ndflarps@email.com\ncancelled\n"
                + "2019-07-18\nJane Dome\n656463626\nemail@mymail.mail\nadmitted\n2019-07-19\nQuinn Terrot\n385489739\nqtpi@cmail.com\ncancelled\n"
                + "2017-06-21\nDoug\n876321053\nthedougie@thefunkybunch.gov\nadmitted\n2017-06-21\nJack Foxx\n222222222\nFoxxie5055@cmail.com\n"
                + "cancelled\n2010-02-21\nBobbie Jean\n876333333\nbj@mymail.org\nadmitted\n2010-06-21\n";
        try {
            studentAdmissionList.add("Bobbie Jean", "876333333", "bj@mymail.org", "admitted", LocalDate.of(2010, 6, 21));
        } catch (DuplicateStudentAdmissionException e) {
            System.out.println(e.getMessage());
            e.printStackTrace();
        }
        assertEquals(expected, studentAdmissionList.allToString());
    }
    
    
    @Test
    public void testAddDuplicate() throws DuplicateStudentAdmissionException {
        expectedException.expect(DuplicateStudentAdmissionException.class);
        studentAdmissionList.add(new StudentAdmission(new Student("Doug", "876321053", "thedougie@thefunkybunch.gov"), "admitted", LocalDate.of(2010, 6, 21)));
    }

    
    @Test  
    public void testRemove() {
        String expected1 = "Jane Dome\n903894392\njaneD@yourmail.org\nadmitted\n2019-01-01\nMitch Richmond\n772748212\nMitch@mail.ca\nadmitted\n2018-12-31\n"
                + "Al Slapper\n223223223\nmrslappy@email.com\ncancelled\n2019-01-02\nMarky Mark\n123456789\nmarkymark@thefunkybunch.gov\nadmitted\n2018-06-30"
                + "\nDebbie Flarps\n835199928\ndflarps@email.com\ncancelled\n2019-07-18\nJane Dome\n656463626\nemail@mymail.mail\nadmitted\n2019-07-19\n"
                + "Quinn Terrot\n385489739\nqtpi@cmail.com\ncancelled\n2017-06-21\nDoug\n876321053\nthedougie@thefunkybunch.gov\nadmitted\n2017-06-21\n"
                + "Jack Foxx\n222222222\nFoxxie5055@cmail.com\ncancelled\n2010-02-21\n";
        String expected2 = "Jane Dome\n903894392\njaneD@yourmail.org\nadmitted\n2019-01-01\nMitch Richmond\n772748212\nMitch@mail.ca\nadmitted\n2018-12-31\n"
                + "Al Slapper\n223223223\nmrslappy@email.com\ncancelled\n2019-01-02\nDebbie Flarps\n835199928\ndflarps@email.com\ncancelled\n2019-07-18\n"
                + "Jane Dome\n656463626\nemail@mymail.mail\nadmitted\n2019-07-19\nQuinn Terrot\n385489739\nqtpi@cmail.com\ncancelled\n2017-06-21\nDoug\n"
                + "876321053\nthedougie@thefunkybunch.gov\nadmitted\n2017-06-21\nJack Foxx\n222222222\nFoxxie5055@cmail.com\ncancelled\n2010-02-21\n";
        String expected3 = "Jane Dome\n903894392\njaneD@yourmail.org\nadmitted\n2019-01-01\nMitch Richmond\n772748212\nMitch@mail.ca\nadmitted\n2018-12-31\n"
                + "Al Slapper\n223223223\nmrslappy@email.com\ncancelled\n2019-01-02\nDebbie Flarps\n835199928\ndflarps@email.com\ncancelled\n2019-07-18\n"
                + "Jane Dome\n656463626\nemail@mymail.mail\nadmitted\n2019-07-19\nDoug\n876321053\nthedougie@thefunkybunch.gov\nadmitted\n2017-06-21\n"
                + "Jack Foxx\n222222222\nFoxxie5055@cmail.com\ncancelled\n2010-02-21\n";
        
        try {
            studentAdmissionList.remove(new StudentAdmission(
                    new Student("Jack Maxim", "289403729", "jackMax@mymail.org"), 
                    "admitted", LocalDate.of(2019, 1, 1)));
        } catch (StudentIdNotFoundException e) {
            System.out.println(e.getMessage());
            e.printStackTrace();
        }
        assertEquals(expected1, studentAdmissionList.allToString());
        
        try {
            studentAdmissionList.remove(new Student("Marky Mark", "123456789", 
                    "markymark@thefunkybunch.gov"));

        } catch (StudentIdNotFoundException e) {
            System.out.println(e.getMessage());
            e.printStackTrace();
        }
        assertEquals(expected2, studentAdmissionList.allToString());
        
        try {
            studentAdmissionList.remove("385489739");
        } catch (StudentIdNotFoundException e) {
            System.out.println(e.getMessage());
            e.printStackTrace();
        }
        assertEquals(expected3, studentAdmissionList.allToString());
    }
    
    
    @Test  
    public void testRemoveStudentIdNotInList() throws StudentIdNotFoundException {
        expectedException.expect(StudentIdNotFoundException.class);
        studentAdmissionList.remove("000000000");
    }
    
    
    @Test
    public void testGetAllStudentAdmissions() {
        ArrayList<StudentAdmission> expected = new ArrayList<StudentAdmission>();

        expected.add(new StudentAdmission(new Student("Jack Maxim", "289403729", "jackMax@mymail.org"), "admitted", LocalDate.of(2019, 1, 1)));
        expected.add(new StudentAdmission(new Student("Jane Dome", "903894392", "janeD@yourmail.org"), "admitted", LocalDate.of(2019, 1, 1)));
        expected.add(new StudentAdmission(new Student("Mitch Richmond", "772748212", "Mitch@mail.ca"), "admitted", LocalDate.of(2018, 12, 31)));
        expected.add(new StudentAdmission(new Student("Al Slapper", "223223223", "mrslappy@email.com"), "cancelled", LocalDate.of(2019, 1, 2)));
        expected.add(new StudentAdmission(new Student("Marky Mark", "123456789", "markymark@thefunkybunch.gov"), "admitted", LocalDate.of(2018, 6, 30)));
        expected.add(new StudentAdmission(new Student("Debbie Flarps", "835199928", "dflarps@email.com"), "cancelled", LocalDate.of(2019, 7, 18)));
        expected.add(new StudentAdmission(new Student("Jane Dome", "656463626", "email@mymail.mail"), "admitted", LocalDate.of(2019, 7, 19)));
        expected.add(new StudentAdmission(new Student("Quinn Terrot", "385489739", "qtpi@cmail.com"), "cancelled", LocalDate.of(2017, 6, 21)));
        expected.add(new StudentAdmission(new Student("Doug", "876321053", "thedougie@thefunkybunch.gov"), "admitted", LocalDate.of(2017, 6, 21)));
        expected.add(new StudentAdmission(new Student("Jack Foxx", "222222222", "Foxxie5055@cmail.com"), "cancelled", LocalDate.of(2010, 2, 21)));

        assertTrue(compareStudentAdmissionArrayList(expected, studentAdmissionList.getAllStudentAdmissions()));
    }
    
    
    @Test
    public void testSearchByStudentName() {
        ArrayList<StudentAdmission> expected = new ArrayList<StudentAdmission>();
        assertTrue(compareStudentAdmissionArrayList(expected, studentAdmissionList.searchByStudentName("SomeName")));
        
        expected.add(new StudentAdmission(new Student("Jane Dome", "903894392", "janeD@yourmail.org"), "admitted", LocalDate.of(2019, 1, 1)));
        expected.add(new StudentAdmission(new Student("Jane Dome", "656463626", "email@mymail.mail"), "admitted", LocalDate.of(2019, 7, 19)));
        assertTrue(compareStudentAdmissionArrayList(expected, studentAdmissionList.searchByStudentName("Jane Dome")));
    }
    
    
    @Test
    public void testSearchByStudentId() {
        StudentAdmission expected = new StudentAdmission(
                new Student("Debbie Flarps", "835199928", "dflarps@email.com"), 
                "cancelled", LocalDate.of(2019, 7, 18));
        StudentAdmission actual = null;
        
        try {
            actual = studentAdmissionList.searchByStudentId("835199928");
        } catch (StudentIdNotFoundException e) {
            System.out.println(e.getMessage());
            e.printStackTrace();
        }
        assertTrue(actual.studentIdEquals(expected));
    }
    
    
    @Test
    public void testSearchByStudentIdStudentIdNotInList() throws StudentIdNotFoundException {
        expectedException.expect(StudentIdNotFoundException.class);
        studentAdmissionList.searchByStudentId("000000000");
    }
    
    
    @Test
    public void testSearchByAdmissionDate() {
        ArrayList<StudentAdmission> expected = new ArrayList<StudentAdmission>();
        assertTrue(compareStudentAdmissionArrayList(expected, studentAdmissionList.searchByAdmissionDate(LocalDate.of(2000, 1, 1))));
        
        expected.add(new StudentAdmission(new Student("Jack Maxim", "289403729", "jackMax@mymail.org"), "admitted", LocalDate.of(2019, 1, 1)));
        expected.add(new StudentAdmission(new Student("Jane Dome", "903894392", "janeD@yourmail.org"), "admitted", LocalDate.of(2019, 1, 1)));
        assertTrue(compareStudentAdmissionArrayList(expected, studentAdmissionList.searchByAdmissionDate(LocalDate.of(2019, 1, 1))));
    }
    
    
    @Test
    public void testSearchByAdmissionDateRange() {
        ArrayList<StudentAdmission> expected = new ArrayList<StudentAdmission>();
        assertTrue(compareStudentAdmissionArrayList(expected, studentAdmissionList.searchByAdmissionDateRange(LocalDate.of(2000, 1, 1), LocalDate.of(2005, 1, 1))));
        
        expected.add(new StudentAdmission(new Student("Jack Maxim", "289403729", "jackMax@mymail.org"), "admitted", LocalDate.of(2019, 1, 1)));
        expected.add(new StudentAdmission(new Student("Jane Dome", "903894392", "janeD@yourmail.org"), "admitted", LocalDate.of(2019, 1, 1)));
        expected.add(new StudentAdmission(new Student("Al Slapper", "223223223", "mrslappy@email.com"), "cancelled", LocalDate.of(2019, 1, 2)));
        expected.add(new StudentAdmission(new Student("Debbie Flarps", "835199928", "dflarps@email.com"), "cancelled", LocalDate.of(2019, 7, 18)));

        assertTrue(compareStudentAdmissionArrayList(expected, studentAdmissionList.searchByAdmissionDateRange(LocalDate.of(2019, 1, 1), LocalDate.of(2019, 7, 18))));
        assertTrue(compareStudentAdmissionArrayList(expected, studentAdmissionList.searchByAdmissionDateRange(LocalDate.of(2019, 7, 18), LocalDate.of(2019, 1, 1))));
    }
    
    
    @Test
    public void testSearchByAdmissionDateMonth() {
        ArrayList<StudentAdmission> expected = new ArrayList<StudentAdmission>();
        assertTrue(compareStudentAdmissionArrayList(expected, studentAdmissionList.searchByAdmissionDateMonth(9, 2018)));
       
        expected.add(new StudentAdmission(new Student("Quinn Terrot", "385489739", "qtpi@cmail.com"), "cancelled", LocalDate.of(2017, 6, 21)));
        expected.add(new StudentAdmission(new Student("Doug", "876321053", "thedougie@thefunkybunch.gov"), "admitted", LocalDate.of(2017, 6, 21)));
        assertTrue(compareStudentAdmissionArrayList(expected, studentAdmissionList.searchByAdmissionDateMonth(6, 2017)));
    }
    
    
    @Test
    public void testSearchByAdmissionDateYear() {
        ArrayList<StudentAdmission> expected = new ArrayList<StudentAdmission>();
        assertTrue(compareStudentAdmissionArrayList(expected, studentAdmissionList.searchByAdmissionDateYear(2011)));
        
        
        expected.add(new StudentAdmission(new Student("Quinn Terrot", "385489739", "qtpi@cmail.com"), "cancelled", LocalDate.of(2017, 6, 21)));
        expected.add(new StudentAdmission(new Student("Doug", "876321053", "thedougie@thefunkybunch.gov"), "admitted", LocalDate.of(2017, 6, 21)));
        assertTrue(compareStudentAdmissionArrayList(expected, studentAdmissionList.searchByAdmissionDateYear(2017)));
    }
    
    
    @Test
    public void testContainsStudentId() {
        assertTrue(studentAdmissionList.containsStudentId("772748212"));
        assertFalse(studentAdmissionList.containsStudentId("938874421"));
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
