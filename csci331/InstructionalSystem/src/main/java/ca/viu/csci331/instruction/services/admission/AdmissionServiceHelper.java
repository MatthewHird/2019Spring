package ca.viu.csci331.instruction.services.admission;

import java.util.ArrayList;

import ca.viu.csci331.instruction.model.StudentAdmission;

public class AdmissionServiceHelper {
    public static ArrayList<StudentAdmission> admissionTypeFilter(ArrayList<StudentAdmission> unfilteredStudentAdmissionList, String admissionType) {
        if (admissionType.equals("both")) {
            return unfilteredStudentAdmissionList;
        } else if (admissionType.equals("admitted")) {
            return getAdmitted(unfilteredStudentAdmissionList);
        } else if (admissionType.equals("cancelled")) {
            return getCancelled(unfilteredStudentAdmissionList);
        }
        return new ArrayList<StudentAdmission>();
    }   
    
    
    public static ArrayList<StudentAdmission> getAdmitted(ArrayList<StudentAdmission> inList) {
        ArrayList<StudentAdmission> outList = new ArrayList<StudentAdmission>();
        
        for (int i = 0; i < inList.size(); i++) {
            if (inList.get(i).getAdmissionStatus().contentEquals("admitted")) {
                outList.add(inList.get(i));
            }
        }
        return outList;
    }
    
    
    public static ArrayList<StudentAdmission> getCancelled(ArrayList<StudentAdmission> inList) {
        ArrayList<StudentAdmission> outList = new ArrayList<StudentAdmission>();
        
        for (int i = 0; i < inList.size(); i++) {
            if (inList.get(i).getAdmissionStatus().contentEquals("cancelled")) {
                outList.add(inList.get(i));
            }
        }
        return outList;
    }
    
    
    public static String generateStudentId() {
        int studentId = 0;
        
        do {
            studentId = (int) (Math.random() * 999999999);
        } while (studentId < 100000000);
        
        return Integer.toString(studentId);
    }
    
    
    public static String generateStudentReport(ArrayList<StudentAdmission> studentAdmissionArrayList) {
        String formattedString = "";
        
        for (StudentAdmission stuAd : studentAdmissionArrayList) {
            formattedString += String.format("Student Name: %s\nStudent ID: %s\nEmail: %s\nAdmission Status: %s\nAdmission Date: %s\n\n", 
                    stuAd.getStudent().getName(), stuAd.getStudent().getStudentId(), stuAd.getStudent().getEmail(), 
                    stuAd.getAdmissionStatus(), stuAd.getAdmissionDate().toString());
        }
        
        return formattedString;
    }
}
