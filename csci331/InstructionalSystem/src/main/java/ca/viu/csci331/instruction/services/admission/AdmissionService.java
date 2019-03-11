package ca.viu.csci331.instruction.services.admission;

import java.io.IOException;
import java.text.DateFormatSymbols;
import java.time.LocalDate;
import java.util.ArrayList;

import ca.viu.csci331.instruction.exception.DuplicateStudentAdmissionException;
import ca.viu.csci331.instruction.exception.InvalidAdmissionListTextFileSyntaxException;
import ca.viu.csci331.instruction.exception.InvalidStudentEmailException;
import ca.viu.csci331.instruction.exception.InvalidStudentNameException;
import ca.viu.csci331.instruction.exception.StudentIdNotFoundException;
import ca.viu.csci331.instruction.model.Student;
import ca.viu.csci331.instruction.model.StudentAdmission;
import ca.viu.csci331.instruction.persistence.admission.AdmissionPersistence;
import ca.viu.csci331.instruction.retrieval.admission.AdmissionRetrieval;

public class AdmissionService {
    private StudentAdmissionList admissionList;
    
    
    public AdmissionService() {
        admissionList = new StudentAdmissionList();
    }
    
    
    public void loadFromTextFile() throws IOException, InvalidAdmissionListTextFileSyntaxException {
        loadFromTextFile("admissionServiceStudentAdmissionSave.txt");
    }
    
    
    public void loadFromTextFile(String loadFilePath) 
            throws IOException, InvalidAdmissionListTextFileSyntaxException {
        
        admissionList = AdmissionRetrieval.loadStudentAdmissionList(loadFilePath);
    }
    
    
    public void saveToTextFile() throws IOException {
        saveToTextFile("admissionServiceStudentAdmissionSave.txt");
    }
    
    
    public void saveToTextFile(String saveFilePath) throws IOException {
        AdmissionPersistence.saveStudentAdmissionList(saveFilePath, admissionList);
    }
    
    
    public void admitNewStudent(String studentName, String studentEmail) throws InvalidStudentNameException, 
            InvalidStudentEmailException {

        String studentId;

        if (studentName.trim().equals("")) {
            throw new InvalidStudentNameException();
        }        

        if (studentEmail.trim().equals("")) {
            throw new InvalidStudentEmailException();
        }
        
        do {
            studentId = AdmissionServiceHelper.generateStudentId();
        } while (admissionList.containsStudentId(studentId));
        
        try {
            admissionList.add(new StudentAdmission(new Student(studentName, studentId, studentEmail)));
        } catch (DuplicateStudentAdmissionException e) {
            e.printStackTrace();
        }
    }
    
    
    public void cancelExistingStudent(String cancelledStudentId) throws StudentIdNotFoundException {
        admissionList.searchByStudentId(cancelledStudentId).setAdmissionStatus("cancelled");
    }


    public void updateStudentAdmissionName(String studentId, String newStudentName) 
            throws StudentIdNotFoundException, InvalidStudentNameException {
        if (newStudentName.trim().equals("")) {
            throw new InvalidStudentNameException();
        }
        admissionList.searchByStudentId(studentId).getStudent().setName(newStudentName);
    }
    
    
    public void updateStudentAdmissionEmail(String studentId, String newStudentEmail) 
            throws StudentIdNotFoundException, InvalidStudentEmailException {
        if (newStudentEmail.trim().equals("")) {
            throw new InvalidStudentEmailException();
        }
        admissionList.searchByStudentId(studentId).getStudent().setEmail(newStudentEmail);
    }
    
    
    public void updateStudentAdmissionStatus(String studentId, String newAdmissionStatus) 
            throws StudentIdNotFoundException {
        admissionList.searchByStudentId(studentId).setAdmissionStatus(newAdmissionStatus);
    }
    
    public void updateStudentAdmissionDate(String studentId, LocalDate newAdmissionDate) 
            throws StudentIdNotFoundException {
        admissionList.searchByStudentId(studentId).setAdmissionDate(newAdmissionDate);
    }
    
    
    public String getStudentReport() {
        String formattedString = "Report of All Students Admitted:\n\n";
        
        ArrayList<StudentAdmission> searchList = admissionList.getAllStudentAdmissions();
        
        formattedString += AdmissionServiceHelper.generateStudentReport(AdmissionServiceHelper.admissionTypeFilter(searchList, "both"));
        
        return formattedString;
    }

    
    public String getStudentReport(LocalDate admissionDate, String admissionType) {
        String formattedString = String.format("Report of Students Admitted on %s:\n\n", admissionDate.toString());
        
        ArrayList<StudentAdmission> searchList = admissionList.searchByAdmissionDate(admissionDate);
        
        formattedString += AdmissionServiceHelper.generateStudentReport(AdmissionServiceHelper.admissionTypeFilter(searchList, admissionType));
        
        return formattedString;
    }
    
    
    public String getStudentReport(LocalDate admissionDateStart, LocalDate admissionDateEnd, String admissionType) {
        String formattedString = String.format("Report of Students Admitted between %s and %s:\n\n", 
                admissionDateStart.toString(), admissionDateEnd.toString());
        
        ArrayList<StudentAdmission> searchList = admissionList.searchByAdmissionDateRange(admissionDateStart, admissionDateEnd);
        
        formattedString += AdmissionServiceHelper.generateStudentReport(AdmissionServiceHelper.admissionTypeFilter(searchList, admissionType));
        
        return formattedString;
    }
    
    
    public String getStudentReport(int admissionMonth, int admissionYear, String admissionType) {
        String formattedString = String.format("Report of Students Admitted during %s of %d:\n\n", 
               new DateFormatSymbols().getMonths()[admissionMonth - 1], admissionYear);
        
        ArrayList<StudentAdmission> searchList = admissionList.searchByAdmissionDateMonth(admissionMonth, admissionYear);
        
        formattedString += AdmissionServiceHelper.generateStudentReport(AdmissionServiceHelper.admissionTypeFilter(searchList, admissionType));
        
        return formattedString;
    }
    
    
    public String getStudentReport(int admissionYear, String admissionType) {
        String formattedString = String.format("Report of Students Admitted during the year %d:\n\n", admissionYear);
        
        ArrayList<StudentAdmission> searchList = admissionList.searchByAdmissionDateYear(admissionYear);
        
        formattedString += AdmissionServiceHelper.generateStudentReport(AdmissionServiceHelper.admissionTypeFilter(searchList, admissionType));
        
        return formattedString;
    }
    
    
    public ArrayList<StudentAdmission> searchAll() {
        return admissionList.getAllStudentAdmissions();
    }
    
    
    public StudentAdmission searchForStudentAdmissionByStudentId(String studentId) throws StudentIdNotFoundException {
        return admissionList.searchByStudentId(studentId);
    }
    
    
    public ArrayList<StudentAdmission> searchForStudentAdmissionsByStudentName(String studentName, String admissionType) {
        return AdmissionServiceHelper.admissionTypeFilter(admissionList.searchByStudentName(studentName), admissionType);
    }
    
    
    public ArrayList<StudentAdmission> searchForStudentAdmissionsByAdmissionDate(LocalDate admissionDate, String admissionType) {
        return AdmissionServiceHelper.admissionTypeFilter(admissionList.searchByAdmissionDate(admissionDate), admissionType);
    }
}
