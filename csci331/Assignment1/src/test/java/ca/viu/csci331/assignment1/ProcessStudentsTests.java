package ca.viu.csci331.assignment1;

import org.junit.Before;
import org.junit.Test;

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
        processStudents.addStudent(new Student("Jack Foxx", "222222222", 3.629));
    }
    
    @Test
    public void testAddStudent() {

    }
    
    @Test
    public void testRankStudents() {
        
    }
    
    @Test
    public void testSearchByName() {
        
    }
    
    @Test
    public void testSearchById() {
        
    }
    
    @Test
    public void testTopStudent() {
        
    }
    
    @Test
    public void testLastStudent() {
        
    }
    
    @Test
    public void testRemoveById() {
        
    }
}
