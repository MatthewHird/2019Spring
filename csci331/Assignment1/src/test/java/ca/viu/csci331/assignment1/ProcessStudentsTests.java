package ca.viu.csci331.assignment1;

import static org.junit.Assert.assertEquals;
import java.io.ByteArrayOutputStream;
import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.PrintStream;
import org.junit.Before;
import org.junit.Test;

/**
 * Collection of test cases for <ProcessStudents>
 * 
 * @author Matthew Hird
 * @date Feb. 3, 2019
 */
public class ProcessStudentsTests {
    private ProcessStudents processStudents;
    
    @Before
    public void populateProcessStudents() {
        processStudents = new ProcessStudents(10);
        processStudents.addStudent(new Student("Jack Maxim", "289403729", 3.4445));
        processStudents.addStudent(new Student("Jane Dome", "903894392", 4.3333));
        processStudents.addStudent(new Student("Mitch Richmond", "772748212", 2.11));
        processStudents.addStudent(new Student("Al Slapper", "223223223", 1.0));
        processStudents.addStudent(new Student("Marky Mark", "123456789", 2.01));
        processStudents.addStudent(new Student("Debbie Flarps", "835199928", 3.0));
        processStudents.addStudent(new Student("Tim van Nerd", "656463626", 4.00));
        processStudents.addStudent(new Student("Quinn Terrot", "385489739", 0.5));
        processStudents.addStudent(new Student("Doug", "876321053", 2.75));
    }
    
    @Test
    public void testAddStudent() {     
        processStudents.addStudent(new Student("Jack Foxx", "222222222", 3.629));
        String expectedPrintout = "-------------------\nStudent Information\n-------------------\n\n"
                + "Student Name: Jack Maxim\nStudent ID:   289403729  GPA: 3.44\n\nStudent Name: "
                + "Jane Dome\nStudent ID:   903894392  GPA: 4.33\n\nStudent Name: Mitch Richmond\n"
                + "Student ID:   772748212  GPA: 2.11\n\nStudent Name: Al Slapper\nStudent ID:   "
                + "223223223  GPA: 1.00\n\nStudent Name: Marky Mark\nStudent ID:   123456789  GPA: 2.01\n"
                + "\nStudent Name: Debbie Flarps\nStudent ID:   835199928  GPA: 3.00\n\n"
                + "Student Name: Tim van Nerd\nStudent ID:   656463626  GPA: 4.00\n\nStudent Name: "
                + "Quinn Terrot\nStudent ID:   385489739  GPA: 0.50\n\nStudent Name: Doug\nStudent ID:   "
                + "876321053  GPA: 2.75\n\nStudent Name: Jack Foxx\nStudent ID:   222222222  GPA: 3.63\n"
                + "\n-------------------\n\n";
        
        ByteArrayOutputStream actualPrintoutStream = new ByteArrayOutputStream();
        System.setOut(new PrintStream(actualPrintoutStream));
        
        processStudents.showStudents();
        assertEquals(expectedPrintout, actualPrintoutStream.toString());

        System.setOut(new PrintStream(new FileOutputStream(FileDescriptor.out)));
    }
    
    @Test
    public void testRankStudents() {
        processStudents.rankStudents();
        String expectedPrintout = "-------------------\nStudent Information\n-------------------\n\n" 
                + "Student Name: Jane Dome\nStudent ID:   903894392  GPA: 4.33\n\nStudent Name: "
                + "Tim van Nerd\nStudent ID:   656463626  GPA: 4.00\n\nStudent Name: Jack Maxim\n"
                + "Student ID:   289403729  GPA: 3.44\n\nStudent Name: Debbie Flarps\nStudent ID:   "
                + "835199928  GPA: 3.00\n\nStudent Name: Doug\nStudent ID:   876321053  GPA: 2.75\n\nStudent "
                + "Name: Mitch Richmond\nStudent ID:   772748212  GPA: 2.11\n\nStudent Name: Marky "
                + "Mark\nStudent ID:   123456789  GPA: 2.01\n\nStudent Name: Al Slapper\nStudent ID:   "
                + "223223223  GPA: 1.00\n\nStudent Name: Quinn Terrot\nStudent ID:   385489739  GPA: "
                + "0.50\n\n-------------------\n\n";
        
        ByteArrayOutputStream actualPrintoutStream = new ByteArrayOutputStream();
        System.setOut(new PrintStream(actualPrintoutStream));
        
        processStudents.showStudents();
        assertEquals(expectedPrintout, actualPrintoutStream.toString());

        System.setOut(new PrintStream(new FileOutputStream(FileDescriptor.out)));
    }
    
    @Test
    public void testSearchByName() {
        Student actualStudent = processStudents.searchByName("Al Slapper");
        
        assertEquals("Al Slapper", actualStudent.getName());
        assertEquals("223223223", actualStudent.getId());
        assertEquals(1.0, actualStudent.getGpa(), 0.0001);
    }
    
    @Test
    public void testSearchById() {
        Student actualStudent = processStudents.searchById("876321053");
        
        assertEquals("Doug", actualStudent.getName());
        assertEquals("876321053", actualStudent.getId());
        assertEquals(2.75, actualStudent.getGpa(), 0.001);
    }
    
    @Test
    public void testTopStudent() {
        Student actualStudent = processStudents.topStudent();
        
        assertEquals("Jane Dome", actualStudent.getName());
        assertEquals("903894392", actualStudent.getId());
        assertEquals(4.3333, actualStudent.getGpa(), 0.001);
        
    }
    
    @Test
    public void testLastStudent() {
        Student actualStudent = processStudents.lastStudent();
        
        assertEquals("Quinn Terrot", actualStudent.getName());
        assertEquals("385489739", actualStudent.getId());
        assertEquals(0.5, actualStudent.getGpa(), 0.001);
    }
    
    @Test
    public void testRemoveById() {
        processStudents.removeById("835199928");
        String expectedPrintout = "-------------------\nStudent Information\n-------------------\n\n"
                + "Student Name: Jack Maxim\nStudent ID:   289403729  GPA: 3.44\n\nStudent Name: "
                + "Jane Dome\nStudent ID:   903894392  GPA: 4.33\n\nStudent Name: Mitch Richmond\n"
                + "Student ID:   772748212  GPA: 2.11\n\nStudent Name: Al Slapper\nStudent ID:   "
                + "223223223  GPA: 1.00\n\nStudent Name: Marky Mark\nStudent ID:   123456789  GPA: 2.01\n"
                + "\nStudent Name: Tim van Nerd\nStudent ID:   656463626  GPA: 4.00\n\nStudent Name: "
                + "Quinn Terrot\nStudent ID:   385489739  GPA: 0.50\n\nStudent Name: Doug\nStudent ID:   "
                + "876321053  GPA: 2.75\n\n-------------------\n\n";
        
        ByteArrayOutputStream actualPrintoutStream = new ByteArrayOutputStream();
        System.setOut(new PrintStream(actualPrintoutStream));
        
        processStudents.showStudents();
        assertEquals(expectedPrintout, actualPrintoutStream.toString());

        System.setOut(new PrintStream(new FileOutputStream(FileDescriptor.out)));
    }
}
