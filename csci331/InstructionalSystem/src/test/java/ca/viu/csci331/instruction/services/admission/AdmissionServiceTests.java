package ca.viu.csci331.instruction.services.admission;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import ca.viu.csci331.instruction.exception.DuplicateStudentAdmissionException;
import ca.viu.csci331.instruction.exception.InvalidAdmissionListTextFileSyntaxException;
import ca.viu.csci331.instruction.exception.InvalidStudentEmailException;
import ca.viu.csci331.instruction.exception.InvalidStudentNameException;
import ca.viu.csci331.instruction.exception.StudentIdNotFoundException;
import ca.viu.csci331.instruction.model.Student;
import ca.viu.csci331.instruction.model.StudentAdmission;

public class AdmissionServiceTests {
    private AdmissionService admissionService;
    
    
    @Rule
    public ExpectedException expectedException = ExpectedException.none();
    
    
    @Test
    public void testLoadFromTextFile() throws IOException, InvalidAdmissionListTextFileSyntaxException {
        ArrayList<StudentAdmission> expected = new ArrayList<StudentAdmission>(); 
        
        expected.add(new StudentAdmission(new Student("Mitch Richmond", "772748212", "Mitch@mail.ca"), "admitted", LocalDate.of(2018, 12, 31)));
        expected.add(new StudentAdmission(new Student("Al Slapper", "223223223", "mrslappy@email.com"), "cancelled", LocalDate.of(2019, 1, 2)));

        
        String saveData = "2\nMitch Richmond\n772748212\nMitch@mail.ca\nadmitted"
                + "\n2018-12-31\nAl Slapper\n223223223\nmrslappy@email.com"
                + "\ncancelled\n2019-01-02\n";
        
        String loadFilePath = "";

        try {
            File tempFile = File.createTempFile("admissionServiceData", null);
            tempFile.deleteOnExit();
            loadFilePath = tempFile.getAbsolutePath();
            
            FileWriter fileWriter = null;

            fileWriter = new FileWriter(tempFile);
            fileWriter.write(saveData);
            fileWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        AdmissionService testAdmissionService = new AdmissionService();
        testAdmissionService.loadFromTextFile(loadFilePath);
        
        assertTrue(compareStudentAdmissionArrayList(expected, testAdmissionService.searchAll()));
    }
    
    
    @Test
    public void testSaveToTextFile() throws IOException {
        populateAdmissionService();
        String expected = "10\nJack Maxim\n289403729\njackMax@mymail.org\nadmitted\n2019-01-01\nJane Dome\n903894392\njaneD@yourmail.org\nadmitted\n"
                + "2019-01-01\nMitch Richmond\n772748212\nMitch@mail.ca\nadmitted\n2018-12-31\nAl Slapper\n223223223\nmrslappy@email.com\ncancelled\n"
                + "2019-01-02\nMarky Mark\n123456789\nmarkymark@thefunkybunch.gov\nadmitted\n2018-06-30\nDebbie Flarps\n835199928\ndflarps@email.com\n"
                + "cancelled\n2019-07-18\nJane Dome\n656463626\nemail@mymail.mail\ncancelled\n2019-07-19\nQuinn Terrot\n385489739\nqtpi@cmail.com\n"
                + "cancelled\n2017-06-21\nDoug\n876321053\nthedougie@thefunkybunch.gov\nadmitted\n2017-06-21\nJack Foxx\n222222222\nFoxxie5055@cmail.com\n"
                + "cancelled\n2010-02-21\n";
        
        File tempFile = File.createTempFile("admissionServiceData", null);
        tempFile.deleteOnExit();
        String tempFilePath = tempFile.getAbsolutePath();
        
        admissionService.saveToTextFile(tempFilePath);
        assertEquals(expected, new String(Files.readAllBytes(tempFile.toPath())));
    }
    
    
    @Test
    public void testAdmitNewStudent() throws InvalidStudentNameException, DuplicateStudentAdmissionException, InvalidStudentEmailException {
        populateAdmissionService();
        admissionService.admitNewStudent("Mr Rogers", "srogers@thehood.org");
        
        ArrayList<StudentAdmission> expected = new ArrayList<StudentAdmission>();
        
        expected.add(new StudentAdmission(new Student("Jack Maxim", "289403729", "jackMax@mymail.org"), "admitted", LocalDate.of(2019, 1, 1)));
        expected.add(new StudentAdmission(new Student("Jane Dome", "903894392", "janeD@yourmail.org"), "admitted", LocalDate.of(2019, 1, 1)));
        expected.add(new StudentAdmission(new Student("Mitch Richmond", "772748212", "Mitch@mail.ca"), "admitted", LocalDate.of(2018, 12, 31)));
        expected.add(new StudentAdmission(new Student("Al Slapper", "223223223", "mrslappy@email.com"), "cancelled", LocalDate.of(2019, 1, 2)));
        expected.add(new StudentAdmission(new Student("Marky Mark", "123456789", "markymark@thefunkybunch.gov"), "admitted", LocalDate.of(2018, 6, 30)));
        expected.add(new StudentAdmission(new Student("Debbie Flarps", "835199928", "dflarps@email.com"), "cancelled", LocalDate.of(2019, 7, 18)));
        expected.add(new StudentAdmission(new Student("Jane Dome", "656463626", "email@mymail.mail"), "cancelled", LocalDate.of(2019, 7, 19)));
        expected.add(new StudentAdmission(new Student("Quinn Terrot", "385489739", "qtpi@cmail.com"), "cancelled", LocalDate.of(2017, 6, 21)));
        expected.add(new StudentAdmission(new Student("Doug", "876321053", "thedougie@thefunkybunch.gov"), "admitted", LocalDate.of(2017, 6, 21)));
        expected.add(new StudentAdmission(new Student("Jack Foxx", "222222222", "Foxxie5055@cmail.com"), "cancelled", LocalDate.of(2010, 2, 21)));
        expected.add(admissionService.searchForStudentAdmissionsByStudentName("Mr Rogers", "admitted").get(0));
        
        assertTrue(compareStudentAdmissionArrayList(expected, admissionService.searchAll()));
    }
    
    
    @Test
    public void testAdmitNewStudentInvalidStudentName() throws InvalidStudentNameException, InvalidStudentEmailException {
        populateAdmissionService();
        expectedException.expect(InvalidStudentNameException.class);
        admissionService.admitNewStudent("   ", "srogers@thehood.org");
    }
    
    
    @Test
    public void testAdmitNewStudentInvalidStudentEmail() throws InvalidStudentEmailException, InvalidStudentNameException {
        populateAdmissionService();
        expectedException.expect(InvalidStudentEmailException.class);
        admissionService.admitNewStudent("Mr Rogers", "   ");
    }
    
    
    @Test
    public void testCancelExistingStudent() throws StudentIdNotFoundException {
        populateAdmissionService();
        assertEquals("admitted", admissionService.searchForStudentAdmissionByStudentId("876321053").getAdmissionStatus());
        admissionService.cancelExistingStudent("876321053");
        assertEquals("cancelled", admissionService.searchForStudentAdmissionByStudentId("876321053").getAdmissionStatus());
    }
    
    
    @Test
    public void testCancelExistingStudentStudentIdNotFound() throws StudentIdNotFoundException {
        populateAdmissionService();
        expectedException.expect(StudentIdNotFoundException.class);
        admissionService.cancelExistingStudent("111111111");
    }
    
    
    @Test
    public void testUpdateStudentAdmissionName() throws StudentIdNotFoundException, InvalidStudentNameException {
        populateAdmissionService();
        assertEquals("Marky Mark", admissionService.searchForStudentAdmissionByStudentId("123456789").getStudent().getName());
        admissionService.updateStudentAdmissionName("123456789", "Mark Wahlberg");
        assertEquals("Mark Wahlberg", admissionService.searchForStudentAdmissionByStudentId("123456789").getStudent().getName());
    }
    
    @Test
    public void testUpdateStudentAdmissionNameStudentIdNotFound() throws StudentIdNotFoundException, InvalidStudentNameException {
        populateAdmissionService();
        expectedException.expect(StudentIdNotFoundException.class);
        admissionService.updateStudentAdmissionName("111111111", "Some Name");
    }
    
    
    @Test
    public void testUpdateStudentAdmissionNameInvalidStudentName() throws InvalidStudentNameException, StudentIdNotFoundException {
        populateAdmissionService();
        expectedException.expect(InvalidStudentNameException.class);
        admissionService.updateStudentAdmissionName("222222222", "   ");
    }
    
    
    @Test
    public void testUpdateStudentAdmissionEmail() throws StudentIdNotFoundException, InvalidStudentEmailException {
        populateAdmissionService();
        assertEquals("markymark@thefunkybunch.gov", admissionService.searchForStudentAdmissionByStudentId("123456789").getStudent().getEmail());
        admissionService.updateStudentAdmissionEmail("123456789", "mwahlberg@sad.com");
        assertEquals("mwahlberg@sad.com", admissionService.searchForStudentAdmissionByStudentId("123456789").getStudent().getEmail());
    }
    
    
    @Test
    public void testUpdateStudentAdmissionEmailStudentIdNotFound() throws StudentIdNotFoundException, InvalidStudentEmailException {
        populateAdmissionService();
        expectedException.expect(StudentIdNotFoundException.class);
        admissionService.updateStudentAdmissionEmail("111111111", "some@email.com");
    }
    
    
    @Test
    public void testUpdateStudentAdmissionEmailInvalidEmail() throws InvalidStudentEmailException, StudentIdNotFoundException {
        populateAdmissionService();
        expectedException.expect(InvalidStudentEmailException.class);
        admissionService.updateStudentAdmissionEmail("222222222", "   ");
    }
    
    
    @Test
    public void testUpdateStudentAdmissionStatus() throws StudentIdNotFoundException {
        populateAdmissionService();
        assertEquals("admitted", admissionService.searchForStudentAdmissionByStudentId("123456789").getAdmissionStatus());
        admissionService.updateStudentAdmissionStatus("123456789", "cancelled");
        assertEquals("cancelled", admissionService.searchForStudentAdmissionByStudentId("123456789").getAdmissionStatus());
    }
    
    
    @Test
    public void testUpdateStudentAdmissionStatusStudentIdNotFound() throws StudentIdNotFoundException {
        populateAdmissionService();
        expectedException.expect(StudentIdNotFoundException.class);
        admissionService.updateStudentAdmissionStatus("111111111", "cancelled");
    }
    
    
    @Test
    public void testUpdateStudentAdmissionDate() throws StudentIdNotFoundException {
        populateAdmissionService();
        assertEquals(LocalDate.of(2018, 6, 30), admissionService.searchForStudentAdmissionByStudentId("123456789").getAdmissionDate());
        admissionService.updateStudentAdmissionDate("123456789", LocalDate.of(2019, 1, 1));
        assertEquals(LocalDate.of(2019, 1, 1), admissionService.searchForStudentAdmissionByStudentId("123456789").getAdmissionDate());
    
    }
    
    
    @Test
    public void testUpdateStudentAdmissionDateStudentIdNotFound() throws StudentIdNotFoundException {
        populateAdmissionService();
        expectedException.expect(StudentIdNotFoundException.class);
        admissionService.updateStudentAdmissionDate("111111111", LocalDate.now());
    }
    
    
    @Test
    public void testGetStudentReportByDate() {
        populateAdmissionService();
        String expected1 = "Report of Students Admitted on 2019-01-01:\n\nStudent Name: Jack Maxim\n" 
                + "Student ID: 289403729\nEmail: jackMax@mymail.org\nAdmission Status: admitted\n"
                + "Admission Date: 2019-01-01\n\nStudent Name: Jane Dome\nStudent ID: 903894392\n"
                + "Email: janeD@yourmail.org\nAdmission Status: admitted\nAdmission Date: 2019-01-01\n\n";
        String expected2 = "Report of Students Admitted on 2019-01-01:\n\nStudent Name: Jack Maxim\n" 
                + "Student ID: 289403729\nEmail: jackMax@mymail.org\nAdmission Status: admitted\n"
                + "Admission Date: 2019-01-01\n\nStudent Name: Jane Dome\nStudent ID: 903894392\n"
                + "Email: janeD@yourmail.org\nAdmission Status: admitted\nAdmission Date: 2019-01-01\n\n";
        String expected3 = "Report of Students Admitted on 2019-01-01:\n\n";
        
        assertEquals(expected1, admissionService.getStudentReport(LocalDate.of(2019, 1, 1), "both"));
        assertEquals(expected2, admissionService.getStudentReport(LocalDate.of(2019, 1, 1), "admitted"));
        assertEquals(expected3, admissionService.getStudentReport(LocalDate.of(2019, 1, 1), "cancelled"));
    }
    
    
    @Test
    public void testGetStudentReportByDateRange() {
        populateAdmissionService();
        String expected1 = "Report of Students Admitted between 2019-01-01 and 2019-07-18:\n\n"
                + "Student Name: Jack Maxim\nStudent ID: 289403729\nEmail: jackMax@mymail.org\n" 
                + "Admission Status: admitted\nAdmission Date: 2019-01-01\n\n" 
                + "Student Name: Jane Dome\nStudent ID: 903894392\nEmail: janeD@yourmail.org\n" 
                + "Admission Status: admitted\nAdmission Date: 2019-01-01\n\n" 
                + "Student Name: Al Slapper\nStudent ID: 223223223\nEmail: mrslappy@email.com\n"
                + "Admission Status: cancelled\nAdmission Date: 2019-01-02\n\n"
                + "Student Name: Debbie Flarps\nStudent ID: 835199928\nEmail: dflarps@email.com\n" 
                + "Admission Status: cancelled\nAdmission Date: 2019-07-18\n\n";
        String expected2 = "Report of Students Admitted between 2019-01-01 and 2019-07-18:\n\n"
                + "Student Name: Jack Maxim\nStudent ID: 289403729\nEmail: jackMax@mymail.org\n" 
                + "Admission Status: admitted\nAdmission Date: 2019-01-01\n\n" 
                + "Student Name: Jane Dome\nStudent ID: 903894392\nEmail: janeD@yourmail.org\n" 
                + "Admission Status: admitted\nAdmission Date: 2019-01-01\n\n";
        String expected3 = "Report of Students Admitted between 2019-01-01 and 2019-07-18:\n\n" 
                + "Student Name: Al Slapper\nStudent ID: 223223223\nEmail: mrslappy@email.com\n"
                + "Admission Status: cancelled\nAdmission Date: 2019-01-02\n\n" 
                + "Student Name: Debbie Flarps\nStudent ID: 835199928\nEmail: dflarps@email.com\n" 
                + "Admission Status: cancelled\nAdmission Date: 2019-07-18\n\n";

        assertEquals(expected1, admissionService.getStudentReport(LocalDate.of(2019, 1, 1), LocalDate.of(2019, 7, 18), "both"));
        assertEquals(expected2, admissionService.getStudentReport(LocalDate.of(2019, 1, 1), LocalDate.of(2019, 7, 18), "admitted"));
        assertEquals(expected3, admissionService.getStudentReport(LocalDate.of(2019, 1, 1), LocalDate.of(2019, 7, 18), "cancelled"));
    }
    
    
    @Test
    public void testGetStudentReportByMonth() {
        populateAdmissionService();
        String expected1 = "Report of Students Admitted during June of 2017:\n\n" 
                + "Student Name: Quinn Terrot\nStudent ID: 385489739\n" 
                + "Email: qtpi@cmail.com\nAdmission Status: cancelled\n" 
                + "Admission Date: 2017-06-21\n\nStudent Name: Doug\n" 
                + "Student ID: 876321053\nEmail: thedougie@thefunkybunch.gov\n" 
                + "Admission Status: admitted\nAdmission Date: 2017-06-21\n\n";
        String expected2 = "Report of Students Admitted during June of 2017:\n\n" 
                + "Student Name: Doug\nStudent ID: 876321053\n" 
                + "Email: thedougie@thefunkybunch.gov\nAdmission Status: admitted\n"
                + "Admission Date: 2017-06-21\n\n";
        String expected3 = "Report of Students Admitted during June of 2017:\n\n" 
                + "Student Name: Quinn Terrot\nStudent ID: 385489739\n" 
                + "Email: qtpi@cmail.com\nAdmission Status: cancelled\n" 
                + "Admission Date: 2017-06-21\n\n";

        assertEquals(expected1, admissionService.getStudentReport(6, 2017, "both"));
        assertEquals(expected2, admissionService.getStudentReport(6, 2017, "admitted"));
        assertEquals(expected3, admissionService.getStudentReport(6, 2017, "cancelled"));
    }
    
    
    @Test
    public void testGetStudentReportByYear() {
        populateAdmissionService();
        String expected1 = "Report of Students Admitted during the year 2017:\n\n" 
                + "Student Name: Quinn Terrot\nStudent ID: 385489739\n" 
                + "Email: qtpi@cmail.com\nAdmission Status: cancelled\n" 
                + "Admission Date: 2017-06-21\n\nStudent Name: Doug\n" 
                + "Student ID: 876321053\nEmail: thedougie@thefunkybunch.gov\n" 
                + "Admission Status: admitted\nAdmission Date: 2017-06-21\n\n";
        String expected2 = "Report of Students Admitted during the year 2017:\n\n" 
                + "Student Name: Doug\nStudent ID: 876321053\n" 
                + "Email: thedougie@thefunkybunch.gov\nAdmission Status: admitted\n"
                + "Admission Date: 2017-06-21\n\n";
        String expected3 = "Report of Students Admitted during the year 2017:\n\n" 
                + "Student Name: Quinn Terrot\nStudent ID: 385489739\n" 
                + "Email: qtpi@cmail.com\nAdmission Status: cancelled\n" 
                + "Admission Date: 2017-06-21\n\n";

        assertEquals(expected1, admissionService.getStudentReport(2017, "both"));
        assertEquals(expected2, admissionService.getStudentReport(2017, "admitted"));
        assertEquals(expected3, admissionService.getStudentReport(2017, "cancelled"));
    }
    
    
    @Test
    public void testSearchForStudentByStudentId() throws StudentIdNotFoundException {
        populateAdmissionService();
        StudentAdmission expected = new StudentAdmission(new Student("Doug", "876321053", "thedougie@thefunkybunch.gov"), "admitted", LocalDate.of(2017, 6, 21));
        assertTrue(expected.studentIdEquals(admissionService.searchForStudentAdmissionByStudentId("876321053")));
    }
    
    
    @Test
    public void testSearchForStudentByStudentIdStudentIdNotFound() throws StudentIdNotFoundException {
        populateAdmissionService();
        expectedException.expect(StudentIdNotFoundException.class);
        admissionService.cancelExistingStudent("111111111");
    }
    
    
    @Test
    public void testSearchForStudentByStudentName() {
        populateAdmissionService();
        
        ArrayList<StudentAdmission> expected = new ArrayList<StudentAdmission>();
        assertTrue(compareStudentAdmissionArrayList(expected, admissionService.searchForStudentAdmissionsByStudentName("SomeName", "both")));
        
        expected.add(new StudentAdmission(new Student("Jane Dome", "903894392", "janeD@yourmail.org"), "admitted", LocalDate.of(2019, 1, 1)));
        expected.add(new StudentAdmission(new Student("Jane Dome", "656463626", "email@mymail.mail"), "admitted", LocalDate.of(2019, 7, 19)));
        assertTrue(compareStudentAdmissionArrayList(expected, admissionService.searchForStudentAdmissionsByStudentName("Jane Dome", "both")));
        
        expected = new ArrayList<StudentAdmission>();
        expected.add(new StudentAdmission(new Student("Jane Dome", "903894392", "janeD@yourmail.org"), "admitted", LocalDate.of(2019, 1, 1)));
        assertTrue(compareStudentAdmissionArrayList(expected, admissionService.searchForStudentAdmissionsByStudentName("Jane Dome", "admitted")));
        
        expected = new ArrayList<StudentAdmission>();
        expected.add(new StudentAdmission(new Student("Jane Dome", "656463626", "email@mymail.mail"), "admitted", LocalDate.of(2019, 7, 19)));
        assertTrue(compareStudentAdmissionArrayList(expected, admissionService.searchForStudentAdmissionsByStudentName("Jane Dome", "cancelled")));
    }
    
    
    @Test
    public void testSearchForStudentByAdmissionDate() {
        populateAdmissionService();
        
        ArrayList<StudentAdmission> expected = new ArrayList<StudentAdmission>();
        assertTrue(compareStudentAdmissionArrayList(expected, admissionService.searchForStudentAdmissionsByAdmissionDate(LocalDate.of(2019, 1, 1), "cancelled")));
        
        expected.add(new StudentAdmission(new Student("Jack Maxim", "289403729", "jackMax@mymail.org"), "admitted", LocalDate.of(2019, 1, 1)));
        expected.add(new StudentAdmission(new Student("Jane Dome", "903894392", "janeD@yourmail.org"), "admitted", LocalDate.of(2019, 1, 1)));
        assertTrue(compareStudentAdmissionArrayList(expected, admissionService.searchForStudentAdmissionsByAdmissionDate(LocalDate.of(2019, 1, 1), "admitted")));
        assertTrue(compareStudentAdmissionArrayList(expected, admissionService.searchForStudentAdmissionsByAdmissionDate(LocalDate.of(2019, 1, 1), "both")));
    }
    
    
    private void populateAdmissionService() {
        String saveData = "10\nJack Maxim\n289403729\njackMax@mymail.org\nadmitted\n2019-01-01\nJane Dome\n903894392\njaneD@yourmail.org\nadmitted\n"
                + "2019-01-01\nMitch Richmond\n772748212\nMitch@mail.ca\nadmitted\n2018-12-31\nAl Slapper\n223223223\nmrslappy@email.com\ncancelled\n"
                + "2019-01-02\nMarky Mark\n123456789\nmarkymark@thefunkybunch.gov\nadmitted\n2018-06-30\nDebbie Flarps\n835199928\ndflarps@email.com\n"
                + "cancelled\n2019-07-18\nJane Dome\n656463626\nemail@mymail.mail\ncancelled\n2019-07-19\nQuinn Terrot\n385489739\nqtpi@cmail.com\n"
                + "cancelled\n2017-06-21\nDoug\n876321053\nthedougie@thefunkybunch.gov\nadmitted\n2017-06-21\nJack Foxx\n222222222\nFoxxie5055@cmail.com\n"
                + "cancelled\n2010-02-21\n";
        
        String loadFilePath = "";

        try {
            File tempFile = File.createTempFile("admissionServiceData", null);
            tempFile.deleteOnExit();
            loadFilePath = tempFile.getAbsolutePath();
            
            FileWriter fileWriter = null;

            fileWriter = new FileWriter(tempFile);
            fileWriter.write(saveData);
            fileWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        admissionService = new AdmissionService();
        try {
            admissionService.loadFromTextFile(loadFilePath);
        } catch (IOException | InvalidAdmissionListTextFileSyntaxException e) {
            e.printStackTrace();
        }
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



















