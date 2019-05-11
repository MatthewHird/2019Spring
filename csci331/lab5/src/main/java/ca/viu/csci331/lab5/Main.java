package ca.viu.csci331.lab5;

import java.io.IOException;

public class Main {

    public static void main(String[] args) {
        TextFileProcessor textFileProcessor = new TextFileProcessor("./src/main/resources/original.txt", "./src/main/resources/processed.txt");
        
        textFileProcessor.openInputStream();
        String unprocessedText = "";
        try {
            unprocessedText = textFileProcessor.read();
        } catch (IOException e1) {
            System.out.print("Error: Input stream has not been opened yet\n");
            e1.printStackTrace();
        }
        textFileProcessor.closeInputStream();
        String processedText = textFileProcessor.process(unprocessedText);
        
        textFileProcessor.openOutputStream();
        try {
            textFileProcessor.write(processedText);
        } catch (IOException e) {
            System.out.print("Error: Output stream has not been opened yet\n");
        }
        textFileProcessor.closeOutputStream();
    }
}
