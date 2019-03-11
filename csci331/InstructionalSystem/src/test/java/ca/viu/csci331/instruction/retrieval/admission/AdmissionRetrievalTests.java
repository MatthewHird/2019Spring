package ca.viu.csci331.instruction.retrieval.admission;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import ca.viu.csci331.instruction.exception.InvalidAdmissionListTextFileSyntaxException;
import ca.viu.csci331.instruction.services.admission.StudentAdmissionList;

public class AdmissionRetrievalTests {
    @Rule
    public ExpectedException expectedException = ExpectedException.none();
    
    
//  @Test
//  public void testAddDuplicate() throws DuplicateStudentAdmissionException {
//      expectedException.expect(DuplicateStudentAdmissionException.class);
//      studentAdmissionList.add(new StudentAdmission(new Student("Doug", "876321053", "thedougie@thefunkybunch.gov"), "admitted", LocalDate.of(2010, 6, 21)));
//  }
    
    
    @Test
    public void testLoadStudentAdmissionList() throws InvalidAdmissionListTextFileSyntaxException, IOException {
        String expected = "Mitch Richmond\n772748212\nMitch@mail.ca\nadmitted\n2018-12-31\nAl Slapper\n223223223\nmrslappy@email.com\ncancelled\n2019-01-02\n";
        String saveData = "2\nMitch Richmond\n772748212\nMitch@mail.ca\nadmitted"
                + "\n2018-12-31\nAl Slapper\n223223223\nmrslappy@email.com"
                + "\ncancelled\n2019-01-02\n";
        
        String loadFilePath = "";
        
        try {
            loadFilePath = prepareLoadFile(saveData);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        StudentAdmissionList admissionList = AdmissionRetrieval.loadStudentAdmissionList(loadFilePath);
        
        assertEquals(expected, admissionList.allToString());
    }
    
    
    @Test
    public void testLoadStudentAdmissionListInvalidEntryCount() throws InvalidAdmissionListTextFileSyntaxException, IOException {
        String saveData = "b\nMitch Richmond\n772748212\nMitch@mail.ca\nadmitted"
                + "\n2018-12-31\nAl Slapper\n223223223\nmrslappy@email.com"
                + "\ncancelled\n2019-01-02\n";
        
        String loadFilePath = "";
        
        try {
            loadFilePath = prepareLoadFile(saveData);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        expectedException.expect(InvalidAdmissionListTextFileSyntaxException.class);
        AdmissionRetrieval.loadStudentAdmissionList(loadFilePath);
    }
    
    
    @Test
    public void testLoadStudentAdmissionListEntryCountFileLengthMismatch() throws InvalidAdmissionListTextFileSyntaxException, IOException {
        String saveData = "3\nMitch Richmond\n772748212\nMitch@mail.ca\nadmitted"
                + "\n2018-12-31\nAl Slapper\n223223223\nmrslappy@email.com"
                + "\ncancelled\n2019-01-02\n";
        
        String loadFilePath = "";
        
        try {
            loadFilePath = prepareLoadFile(saveData);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        expectedException.expect(InvalidAdmissionListTextFileSyntaxException.class);
        AdmissionRetrieval.loadStudentAdmissionList(loadFilePath);
    }
    
    
    @Test
    public void testLoadStudentAdmissionListIOException() throws InvalidAdmissionListTextFileSyntaxException, IOException {
        String loadFilePath = "";

        expectedException.expect(IOException.class);
        AdmissionRetrieval.loadStudentAdmissionList(loadFilePath);
    }
    
    
    @Test
    public void testLoadStudentAdmissionListInvalidStudentName() throws InvalidAdmissionListTextFileSyntaxException, IOException {
        String saveData = "2\n \n772748212\nMitch@mail.ca\nadmitted"
                + "\n2018-12-31\nAl Slapper\n223223223\nmrslappy@email.com"
                + "\ncancelled\n2019-01-02\n";
        
        String loadFilePath = "";
        
        try {
            loadFilePath = prepareLoadFile(saveData);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        expectedException.expect(InvalidAdmissionListTextFileSyntaxException.class);
        AdmissionRetrieval.loadStudentAdmissionList(loadFilePath);
    }
    
    
    @Test
    public void testLoadStudentAdmissionListInvalidStudentIdSyntax() throws InvalidAdmissionListTextFileSyntaxException, IOException {
        String saveData = "2\nMitch Richmond\n072748212\nMitch@mail.ca\nadmitted"
                + "\n2018-12-31\nAl Slapper\n223223223\nmrslappy@email.com"
                + "\ncancelled\n2019-01-02\n";
        
        String loadFilePath = "";
        
        try {
            loadFilePath = prepareLoadFile(saveData);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        expectedException.expect(InvalidAdmissionListTextFileSyntaxException.class);
        AdmissionRetrieval.loadStudentAdmissionList(loadFilePath);
    }
    
    
    @Test
    public void testLoadStudentAdmissionListInvalidDuplicateStudentId() throws InvalidAdmissionListTextFileSyntaxException, IOException {
        String saveData = "2\nMitch Richmond\n223223223\nMitch@mail.ca\nadmitted"
                + "\n2018-12-31\nAl Slapper\n223223223\nmrslappy@email.com"
                + "\ncancelled\n2019-01-02\n";
        
        String loadFilePath = "";
        
        try {
            loadFilePath = prepareLoadFile(saveData);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        expectedException.expect(InvalidAdmissionListTextFileSyntaxException.class);
        AdmissionRetrieval.loadStudentAdmissionList(loadFilePath);
    }
    
    
    @Test
    public void testLoadStudentAdmissionListInvalidStudentEmail() throws InvalidAdmissionListTextFileSyntaxException, IOException {
        String saveData = "2\nMitch Richmond\n772748212\n \nadmitted"
                + "\n2018-12-31\nAl Slapper\n223223223\nmrslappy@email.com"
                + "\ncancelled\n2019-01-02\n";
        
        String loadFilePath = "";
        
        try {
            loadFilePath = prepareLoadFile(saveData);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        expectedException.expect(InvalidAdmissionListTextFileSyntaxException.class);
        AdmissionRetrieval.loadStudentAdmissionList(loadFilePath);
    }
    
    
    @Test
    public void testLoadStudentAdmissionListInvalidAdmissionStatus() throws InvalidAdmissionListTextFileSyntaxException, IOException {
        String saveData = "2\nMitch Richmond\n772748212\nMitch@mail.ca\nadmitted"
                + "\n2018-12-31\nAl Slapper\n223223223\nmrslappy@email.com"
                + "\ncancel\n2019-01-02\n";
        
        String loadFilePath = "";
        
        try {
            loadFilePath = prepareLoadFile(saveData);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        expectedException.expect(InvalidAdmissionListTextFileSyntaxException.class);
        AdmissionRetrieval.loadStudentAdmissionList(loadFilePath);
    }
    
    
    @Test
    public void testLoadStudentAdmissionListInvalidAdmissionDateSyntax() throws InvalidAdmissionListTextFileSyntaxException, IOException {
        String saveData = "2\nMitch Richmond\n772748212\nMitch@mail.ca\nadmitted"
                + "\n2018-12-31\nAl Slapper\n223223223\nmrslappy@email.com"
                + "\ncancelled\n01-02-2019\n";
        
        String loadFilePath = "";
        
        try {
            loadFilePath = prepareLoadFile(saveData);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        expectedException.expect(InvalidAdmissionListTextFileSyntaxException.class);
        AdmissionRetrieval.loadStudentAdmissionList(loadFilePath);
    }
    
    
    private String prepareLoadFile(String saveData) throws IOException {
        File tempFile = File.createTempFile("admissionServiceData", null);
        tempFile.deleteOnExit();
        String tempFilePath = tempFile.getAbsolutePath();
        
        FileWriter fileWriter = null;

        fileWriter = new FileWriter(tempFile);
        fileWriter.write(saveData);
        fileWriter.close();
        
        return tempFilePath;
    }
}
