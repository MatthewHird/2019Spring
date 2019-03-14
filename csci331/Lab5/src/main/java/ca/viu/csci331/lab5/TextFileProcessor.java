package ca.viu.csci331.lab5;

import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;

public class TextFileProcessor {
    private String inputFileName;
    private String outputFileName;
    private InputStream inputStream;
    private OutputStream outputStream;
    
    
    public TextFileProcessor(String inputFilePath, String outputFilePath) {
        inputFileName = inputFilePath;
        outputFileName = outputFilePath;
    }
    
    
    public String getInputFileName() {
        return inputFileName;
    }
    
    
    public void setInputFileName(String inputFileName) {
        this.inputFileName = inputFileName;
    }
    
    
    public String getOutputFileName() {
        return outputFileName;
    }
    
    
    public void setOutputFileName(String outputFileName) {
        this.outputFileName = outputFileName;
    }
    
    
    public InputStream getInputStream() {
        return inputStream;
    }
    
    
    public void setInputStream(InputStream inputStream) {
        this.inputStream = inputStream;
    }
    
    
    public OutputStream getOutputStream() {
        return outputStream;
    }
    
    
    public void setOutputStream(OutputStream outputStream) {
        this.outputStream = outputStream;
    }
    
    
    public void openInputStream() {
        try {
            inputStream = new FileInputStream(inputFileName);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
    
    
    public void closeInputStream() {
        try {
            inputStream.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    
    
    public void openOutputStream() {
        try {
            outputStream = new FileOutputStream(outputFileName);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
    
    
    public void closeOutputStream() {
        try {
            outputStream.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    
    
    public String read() {
        StringBuilder stringBuilder = new StringBuilder();
        
        try {
            Reader reader = new InputStreamReader(inputStream, "UTF-8");
            int ch = reader.read();
            while(ch >= 0) {
                stringBuilder.append((char)ch);
                ch = reader.read();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        return stringBuilder.toString();
    }
    
    
    public void write(String textString) throws IOException {
        Writer outWriter = new BufferedWriter(new OutputStreamWriter(outputStream, "UTF-8"));
        outWriter.write(textString);
        outWriter.close();
    }
    
    
    public String process(String textToProcess) {
        String[] sentences = textToProcess.split("[.][ ]{0,}");
        StringBuilder processedText = new StringBuilder();
        
        int sentenceCount = sentences.length;
        int wordCount = 0;
        
        for (int i = 0; i < sentenceCount; i++) {
            String[] words = sentences[i].trim().split(" ");
            wordCount += words.length;
            
            processedText.append(String.format("Sentence-%d, Word Count-%d\n%s.\n\n", i + 1, words.length, sentences[i]));
            
        }
        
        processedText.append(String.format("Total Sentences: %d, Total Words: %d", sentenceCount, wordCount));
        
        return processedText.toString();
    }
}
