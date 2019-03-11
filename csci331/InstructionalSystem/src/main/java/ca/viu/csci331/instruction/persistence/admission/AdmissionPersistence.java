package ca.viu.csci331.instruction.persistence.admission;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import ca.viu.csci331.instruction.services.admission.StudentAdmissionList;

public class AdmissionPersistence {
    public static void saveStudentAdmissionList(String saveFilePath, 
            StudentAdmissionList admissionList) throws IOException {
        File saveFile = new File(saveFilePath);
        String saveData = Integer.toString(admissionList.getStudentCount()) + "\n"
                + admissionList.allToString();
        
        FileWriter fileWriter = null;

        fileWriter = new FileWriter(saveFile);
        fileWriter.write(saveData);
        fileWriter.close();
    }
}
