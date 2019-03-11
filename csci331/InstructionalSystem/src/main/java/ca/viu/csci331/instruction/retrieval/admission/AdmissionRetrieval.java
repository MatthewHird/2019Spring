package ca.viu.csci331.instruction.retrieval.admission;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.format.DateTimeParseException;
import java.util.List;
import java.util.regex.Pattern;

import ca.viu.csci331.instruction.exception.DuplicateStudentAdmissionException;
import ca.viu.csci331.instruction.exception.InvalidAdmissionListTextFileSyntaxException;
import ca.viu.csci331.instruction.exception.InvalidAdmissionStatusException;
import ca.viu.csci331.instruction.exception.InvalidStudentEmailException;
import ca.viu.csci331.instruction.exception.InvalidStudentIdSyntaxException;
import ca.viu.csci331.instruction.exception.InvalidStudentNameException;
import ca.viu.csci331.instruction.services.admission.StudentAdmissionList;

public class AdmissionRetrieval {
    public static StudentAdmissionList loadStudentAdmissionList(String loadFilePath) 
            throws InvalidAdmissionListTextFileSyntaxException, IOException {
        Path path = Paths.get(loadFilePath);
        List<String> loadFileLines = Files.readAllLines(path);
        
        int entryCount = 0;
        if (Pattern.matches("^[0-9]{1,}$", loadFileLines.get(0))) {
            entryCount = Integer.parseInt(loadFileLines.get(0));
        } else {
            throw new InvalidAdmissionListTextFileSyntaxException("Entry count is not an integer (line 1)");
        }
        
        if (loadFileLines.size() < entryCount * 5 + 1) {
            throw new InvalidAdmissionListTextFileSyntaxException("Entry count does not match file length (line 1)");
        }
        
        StudentAdmissionList admissionList = new StudentAdmissionList();
        
        
        int currentIndex = 0;
        try {
            for (int i = 0; i < entryCount; i++) {
                currentIndex = i * 5 + 1;
                String studentName = loadFileLines.get(currentIndex);
                String studentId = loadFileLines.get(currentIndex + 1);
                String studentEmail = loadFileLines.get(currentIndex + 2);
                String admissionStatus = loadFileLines.get(currentIndex + 3);
                LocalDate admissionDate = LocalDate.parse(loadFileLines.get(currentIndex + 4));
                
                if (studentName.trim().equals("")) {
                    throw new InvalidStudentNameException();
                }
                if (!Pattern.matches("^[1-9][0-9]{8}$", studentId)) {
                    throw new InvalidStudentIdSyntaxException(studentId);
                }
                if (studentEmail.trim().equals("")) {
                    throw new InvalidStudentEmailException();
                }
                if (!admissionStatus.equals("admitted") && !admissionStatus.equals("cancelled")) {
                    throw new InvalidAdmissionStatusException();
                }
                
                admissionList.add(studentName, studentId, studentEmail, admissionStatus, admissionDate);
            }
        } catch (InvalidStudentNameException e) {
            throw new InvalidAdmissionListTextFileSyntaxException(
                    String.format("Syntax error on line %d: Student name may not be blank", currentIndex + 1));
        } catch (InvalidStudentIdSyntaxException e) {
            throw new InvalidAdmissionListTextFileSyntaxException(
                    String.format("Syntax error on line %d: Student ID is invalid.\n"
                            + "Student ID must be 9 characters long, only include characters "
                            + "between 0-9 and not start start with 0", currentIndex + 2));
        } catch (DuplicateStudentAdmissionException e) {
            throw new InvalidAdmissionListTextFileSyntaxException(
                    String.format("Syntax error on line %d: Student ID %s is already in the system", 
                            currentIndex + 2, e.getDuplicateStudentId()));
        } catch (InvalidStudentEmailException e) {
            throw new InvalidAdmissionListTextFileSyntaxException(
                    String.format("Syntax error on line %d: Student email may not be blank", currentIndex + 3));
        } catch (InvalidAdmissionStatusException e) {
            throw new InvalidAdmissionListTextFileSyntaxException(
                    String.format("Syntax error on line %d: Admission status must be \"admitted\" or \"cancelled\"", currentIndex + 4));
        } catch (DateTimeParseException e) {
            throw new InvalidAdmissionListTextFileSyntaxException(
                    String.format("Syntax error on line %d: Admission date must be in the format \"yyyy-mm-dd\"", currentIndex + 5));
        } 
        
        return admissionList;
    }
}
