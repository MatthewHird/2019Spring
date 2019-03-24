package ca.viu.csci331.instruction.model;

import java.time.LocalDate;
import java.util.Comparator;

import ca.viu.csci331.instruction.exception.InvalidEmploymentStatusRuntimeException;

public class InstructorEmployment implements Comparable<Object> {
    private Instructor instructor;
    private String employmentStatus;
    private LocalDate employmentDate;
    
    public InstructorEmployment(Instructor instructor) {
        this.instructor = instructor;
        this.employmentStatus = "employed";
        this.employmentDate = LocalDate.now();
    }
    
    public InstructorEmployment(Instructor instructor, String employmentStatus) {
        this.instructor = instructor;
        this.employmentStatus = employmentStatus;
        this.employmentDate = LocalDate.now();
    }
    
    public InstructorEmployment(Instructor instructor, LocalDate employmentDate) {
        this.instructor = instructor;
        this.employmentStatus = "employed";
        this.employmentDate = employmentDate;
    }
    
    public InstructorEmployment (Instructor instructor, String employmentStatus, LocalDate employmentDate) {
        this.instructor = instructor;
        this.employmentStatus = employmentStatus;
        this.employmentDate = employmentDate;
    }

    public Instructor getInstructor() {
        return instructor;
    }

    public void setInstructor(Instructor instructor) {
        this.instructor = instructor;
    }

    public String getEmploymentStatus() {
        return employmentStatus;
    }
    

    public void setEmploymentStatus(String employmentStatus) {
        if (employmentStatus == "employed" || employmentStatus == "terminated") {
            this.employmentStatus = employmentStatus;
        } else {
            throw new InvalidEmploymentStatusRuntimeException(employmentStatus);
        }
        
    }

    public LocalDate getEmploymentDate() {
        return employmentDate;
    }

    public void setEmploymentDate(LocalDate employmentDate) {
        this.employmentDate = employmentDate;
    }
    
    public boolean instructorIdEquals(Object other) {
        if (other instanceof InstructorEmployment) {
            return this.getInstructor().getInstructorId().equals(((InstructorEmployment) other).getInstructor().getInstructorId());
        } else if (other instanceof Instructor) {
            return this.getInstructor().getInstructorId().equals(((Instructor) other).getInstructorId());
        } else if (other instanceof String) {
            return this.getInstructor().getInstructorId().equals(other);
        }
        return false;
    }
    
    @Override
    public String toString() {
        return String.format("%s%s\n%s\n", getInstructor().toString(), 
                getEmploymentStatus(), getEmploymentDate().toString());
    }
    
    @Override
    public int compareTo(Object o) {
        InstructorEmployment s = (InstructorEmployment) o;
        return this.getInstructor().getInstructorId().compareTo(s.getInstructor().getInstructorId());
    }
    
    static public Comparator<InstructorEmployment> getDateComparator() {
        return  new InstructorEmploymentDateComparator();
    }
    
    private static class InstructorEmploymentDateComparator implements Comparator<InstructorEmployment> {
        @Override
        public int compare(InstructorEmployment o1, InstructorEmployment o2) {
            int result = o1.getEmploymentDate().compareTo(o2.getEmploymentDate());
            if(result == 0) {
                result =  o1.getInstructor().getName().compareToIgnoreCase(o2.getInstructor().getName());
            }
           return result;
        }
    }
}
