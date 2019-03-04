package ca.viu.csci331.instruction.services.admission;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import ca.viu.csci331.instruction.exception.DuplicateStudentAdmissionException;
import ca.viu.csci331.instruction.exception.StudentIdNotFoundException;
import ca.viu.csci331.instruction.model.Student;
import ca.viu.csci331.instruction.model.StudentAdmission;

public class StudentAdmissionList {
    private List<StudentAdmission> studentAdmissions;
//    private int capacity;
//    private int studentCount;
    
    public StudentAdmissionList() {
        studentAdmissions = new ArrayList<StudentAdmission>();
    }
    
    public void add(StudentAdmission addedStudentAdmission) throws DuplicateStudentAdmissionException {
            if (!containsStudentId(addedStudentAdmission.getStudent().getStudentId())) {
                studentAdmissions.add(addedStudentAdmission);
            } else {
                throw new DuplicateStudentAdmissionException(addedStudentAdmission);
            }
    }
    
    public StudentAdmission remove(StudentAdmission removedStudentAdmission) throws StudentIdNotFoundException {
        int removeIndex = studentAdmissions.indexOf(removedStudentAdmission);
        if (removeIndex == -1) {
            throw new StudentIdNotFoundException(removedStudentAdmission.getStudent().getStudentId());
        }
        return studentAdmissions.remove(removeIndex);
    }
    
    @SuppressWarnings("unlikely-arg-type")
    public StudentAdmission remove(Student removedStudent) throws StudentIdNotFoundException {
        int removeIndex = -1;
        for (int i = 0; i < studentAdmissions.size(); i++) {
            if (studentAdmissions.get(i).equals(removedStudent)) {
                removeIndex = i;
                break;
            }
        }
        if (removeIndex == -1) {
            throw new StudentIdNotFoundException(removedStudent.getStudentId());
        }
        return studentAdmissions.remove(removeIndex);
    }
    
    @SuppressWarnings("unlikely-arg-type")
    public StudentAdmission remove(String removedStudentId) throws StudentIdNotFoundException {
        int removeIndex = -1;
        for (int i = 0; i < studentAdmissions.size(); i++) {
            if (studentAdmissions.get(i).equals(removedStudentId)) {
                removeIndex = i;
                break;
            }
        }
        if (removeIndex == -1) {
            throw new StudentIdNotFoundException(removedStudentId);
        }
        return studentAdmissions.remove(removeIndex);
    }
    
    public ArrayList<StudentAdmission> searchByStudentName(String studentName) {
        ArrayList<StudentAdmission> studentAdmissionsWithName = new ArrayList<StudentAdmission>();
        for (StudentAdmission studentAdmission : studentAdmissions) {
            if (studentAdmission.getStudent().getName().equals(studentName)) {
                studentAdmissionsWithName.add(studentAdmission);
            }
        }
        return studentAdmissionsWithName;
    }
    
    @SuppressWarnings("unlikely-arg-type")
    public StudentAdmission searchByStudentId(String studentId) throws StudentIdNotFoundException {
        for (StudentAdmission studentAdmission : studentAdmissions) {
            if (studentAdmission.equals(studentId)) {
                return studentAdmission;
            }
        }
        throw new StudentIdNotFoundException(studentId);
    }
    
    public ArrayList<StudentAdmission> searchByAdmissionDate(LocalDate admissionDate) {
        ArrayList<StudentAdmission> studentAdmissionsWithDate = new ArrayList<StudentAdmission>();
        for (StudentAdmission studentAdmission : studentAdmissions) {
            if (studentAdmission.getAdmissionDate().equals(admissionDate)) {
                studentAdmissionsWithDate.add(studentAdmission);
            }
        }
        return studentAdmissionsWithDate;
    }
    
    public ArrayList<StudentAdmission> searchByAdmissionDateRange(LocalDate admissionDateStart, LocalDate admissionDateEnd) {
        LocalDate rangeStart = null;
        LocalDate rangeEnd = null;
        
        if (admissionDateStart.compareTo(admissionDateEnd) > 0) {
            rangeStart = admissionDateEnd;
            rangeEnd = admissionDateStart;
        } else {
            rangeStart = admissionDateStart;
            rangeEnd = admissionDateEnd;
        }
        
        ArrayList<StudentAdmission> studentAdmissionsWithDateRange = new ArrayList<StudentAdmission>();
        for (StudentAdmission studentAdmission : studentAdmissions) {
            if (studentAdmission.getAdmissionDate().compareTo(rangeStart) >= 0
                    && studentAdmission.getAdmissionDate().compareTo(rangeEnd) <= 0) {
                studentAdmissionsWithDateRange.add(studentAdmission);
            }
        }
        return studentAdmissionsWithDateRange;
    }
    
    public ArrayList<StudentAdmission> searchByAdmissionDateMonth(int admissionMonth) {
        ArrayList<StudentAdmission> studentAdmissionsWithDateMonth = new ArrayList<StudentAdmission>();
        for (StudentAdmission studentAdmission : studentAdmissions) {
            if (studentAdmission.getAdmissionDate().getMonthValue() == admissionMonth) {
                studentAdmissionsWithDateMonth.add(studentAdmission);
            }
        }
        return studentAdmissionsWithDateMonth;
    }
    
    public ArrayList<StudentAdmission> searchByAdmissionDateYear(int admissionYear) {
        ArrayList<StudentAdmission> studentAdmissionsWithDateYear = new ArrayList<StudentAdmission>();
        for (StudentAdmission studentAdmission : studentAdmissions) {
            if (studentAdmission.getAdmissionDate().getYear() == admissionYear) {
                studentAdmissionsWithDateYear.add(studentAdmission);
            }
        }
        return studentAdmissionsWithDateYear;
    }
    
    public String allToString() {
        String asString = "";
        for (StudentAdmission studentAdmission : studentAdmissions) {
            asString += studentAdmission.toString();
        }
        return asString;
    }
    

    
    @SuppressWarnings("unlikely-arg-type")
    public boolean containsStudentId(String testStudentId) {
        for (StudentAdmission studentAdmission : studentAdmissions) {
            if (studentAdmission.equals(testStudentId)) {
                return true;
            }
        }
        return false;
    }
}
