package ca.viu.csci331.lab5;

import java.io.IOException;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class TextFileProcessorTest {
    private TextFileProcessor textFileProcessor;
    
    
    @Before
    public void prepareTextFileProcessor() {
        textFileProcessor = new TextFileProcessor("./src/test/resources/original.txt", "./src/test/resources/processed.txt");
    }
    
    
    @Rule
    public ExpectedException expectedException = ExpectedException.none();
    
    
    @Test
    public void testOpenInputStream() {
        
    }
    
    
    @Test
    public void testCloseInputStream() {
        
    }
    
    
    @Test
    public void testOpenOutputStream() {
        
    }
    
    
    @Test
    public void testCloseOutputStream() {
        
    }
    
    
    @Test
    public void testRead() {
        
    }
    
    
    @Test
    public void testWrite() throws IOException {
        
    }
    
    
    @Test
    public void testWriteUnopenedOutputStream() throws IOException {
        expectedException.expect(IOException.class);
        textFileProcessor.write("test");
    }
    
    
    @Test
    public void testProcess() {
        
    }
}
