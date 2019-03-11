package ca.viu.csci331.instruction.persistence.admission;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.time.LocalDate;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import ca.viu.csci331.instruction.exception.DuplicateStudentAdmissionException;
import ca.viu.csci331.instruction.model.Student;
import ca.viu.csci331.instruction.model.StudentAdmission;
import ca.viu.csci331.instruction.services.admission.StudentAdmissionList;

public class AdmissionPersistenceTests {
    private StudentAdmissionList admissionList;
    
    
    @Rule
    public ExpectedException expectedException = ExpectedException.none();
    
    
    @Before
    public void prepareStudentAdmissionList() {
        admissionList = new StudentAdmissionList();

        try {
            admissionList.add(new StudentAdmission(new Student("Mitch Richmond", "772748212", "Mitch@mail.ca"), "admitted", LocalDate.of(2018, 12, 31)));
            admissionList.add(new StudentAdmission(new Student("Al Slapper", "223223223", "mrslappy@email.com"), "cancelled", LocalDate.of(2019, 1, 2)));
        } catch (DuplicateStudentAdmissionException e) {
            e.printStackTrace();
        }
    }
    

    @Test
    public void testSaveStudentAdmissionList() throws IOException {
        File tempFile = File.createTempFile("admissionServiceData", null);
        tempFile.deleteOnExit();
        String tempFilePath = tempFile.getAbsolutePath();
        
        String expected = "2\nMitch Richmond\n772748212\nMitch@mail.ca\nadmitted"
                + "\n2018-12-31\nAl Slapper\n223223223\nmrslappy@email.com"
                + "\ncancelled\n2019-01-02\n";
        
        AdmissionPersistence.saveStudentAdmissionList(tempFilePath, admissionList);
        
        assertEquals(expected, new String(Files.readAllBytes(tempFile.toPath())));
    }
    
    
    @Test
    public void testSaveStudentAdmissionListIOException() throws IOException {
        expectedException.expect(IOException.class);
        AdmissionPersistence.saveStudentAdmissionList("/", admissionList);
    }
}
