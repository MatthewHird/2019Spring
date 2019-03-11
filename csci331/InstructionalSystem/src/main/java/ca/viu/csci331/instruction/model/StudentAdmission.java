package ca.viu.csci331.instruction.model;

import java.time.LocalDate;

import ca.viu.csci331.instruction.exception.InvalidAdmissionStatusRuntimeException;

public class StudentAdmission implements Comparable<Object> {
    private Student student;
    private String admissionStatus;
    private LocalDate admissionDate;
    
    public StudentAdmission(Student student) {
        this.student = student;
        this.admissionStatus = "admitted";
        this.admissionDate = LocalDate.now();
    }
    
    public StudentAdmission(Student student, String admissionStatus) {
        this.student = student;
        this.admissionStatus = admissionStatus;
        this.admissionDate = LocalDate.now();
    }
    
    public StudentAdmission(Student student, LocalDate admissionDate) {
        this.student = student;
        this.admissionStatus = "admitted";
        this.admissionDate = admissionDate;
    }
    
    public StudentAdmission (Student student, String admissionStatus, LocalDate admissionDate) {
        this.student = student;
        this.admissionStatus = admissionStatus;
        this.admissionDate = admissionDate;
    }

    public Student getStudent() {
        return student;
    }

    public void setStudent(Student student) {
        this.student = student;
    }

    public String getAdmissionStatus() {
        return admissionStatus;
    }
    

    public void setAdmissionStatus(String admissionStatus) {
        if (admissionStatus == "admitted" || admissionStatus == "cancelled") {
            this.admissionStatus = admissionStatus;
        } else {
            throw new InvalidAdmissionStatusRuntimeException(admissionStatus);
        }
        
    }

    public LocalDate getAdmissionDate() {
        return admissionDate;
    }

    public void setAdmissionDate(LocalDate admissionDate) {
        this.admissionDate = admissionDate;
    }
    
    public boolean studentIdEquals(Object other) {
        if (other instanceof StudentAdmission) {
            return this.getStudent().getStudentId().equals(((StudentAdmission) other).getStudent().getStudentId());
        } else if (other instanceof Student) {
            return this.getStudent().getStudentId().equals(((Student) other).getStudentId());
        } else if (other instanceof String) {
            return this.getStudent().getStudentId().equals(other);
        }
        return false;
    }
    
    @Override
    public String toString() {
        return student.toString() + getAdmissionStatus() + "\n" + getAdmissionDate() + "\n";
    }
    
    @Override
    public int compareTo(Object o) {
        StudentAdmission s = (StudentAdmission) o;
        return this.getStudent().getStudentId().compareTo(s.getStudent().getStudentId());
    }
}
