package ca.viu.csci331.instruction.services.administration;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import ca.viu.csci331.instruction.exception.DuplicateInstructorEmploymentException;
import ca.viu.csci331.instruction.exception.InstructorIdNotFoundException;
import ca.viu.csci331.instruction.model.Instructor;
import ca.viu.csci331.instruction.model.InstructorEmployment;

public class InstructorEmploymentList {
    private List<InstructorEmployment> instructorEmployments;
    
    public InstructorEmploymentList() {
        instructorEmployments = new ArrayList<InstructorEmployment>();
    }
    
    public int getInstructorCount() {
        return instructorEmployments.size();
    }
    
    public void add(InstructorEmployment addedInstructorEmployment) throws DuplicateInstructorEmploymentException {
            if (!containsInstructorId(addedInstructorEmployment.getInstructor().getInstructorId())) {
                instructorEmployments.add(addedInstructorEmployment);
            } else {
                throw new DuplicateInstructorEmploymentException(addedInstructorEmployment);
            }
    }
    
    public void add(String instructorName, String instructorId, String instructorEmail, 
            String employmentStatus, LocalDate employmentDate) throws DuplicateInstructorEmploymentException {
        
        this.add(new InstructorEmployment(
                new Instructor(instructorName, instructorId, instructorEmail), employmentStatus, employmentDate ));
    }
    
    public InstructorEmployment remove(InstructorEmployment removedInstructorEmployment) throws InstructorIdNotFoundException {
        int removeIndex = -1;
        for (int i = 0; i < instructorEmployments.size(); i++) {
            if (instructorEmployments.get(i).instructorIdEquals(removedInstructorEmployment)) {
                removeIndex = i;
                break;
            }
        }
        if (removeIndex == -1) {
            throw new InstructorIdNotFoundException(removedInstructorEmployment.getInstructor().getInstructorId());
        }
        return instructorEmployments.remove(removeIndex);
    }
    
    public InstructorEmployment remove(Instructor removedInstructor) throws InstructorIdNotFoundException {
        int removeIndex = -1;
        for (int i = 0; i < instructorEmployments.size(); i++) {
            if (instructorEmployments.get(i).instructorIdEquals(removedInstructor)) {
                removeIndex = i;
                break;
            }
        }
        if (removeIndex == -1) {
            throw new InstructorIdNotFoundException(removedInstructor.getInstructorId());
        }
        return instructorEmployments.remove(removeIndex);
    }
    
    public InstructorEmployment remove(String removedInstructorId) throws InstructorIdNotFoundException {
        int removeIndex = -1;
        for (int i = 0; i < instructorEmployments.size(); i++) {
            if (instructorEmployments.get(i).instructorIdEquals(removedInstructorId)) {
                removeIndex = i;
                break;
            }
        }
        if (removeIndex == -1) {
            throw new InstructorIdNotFoundException(removedInstructorId);
        }
        return instructorEmployments.remove(removeIndex);
    }
    
    public ArrayList<InstructorEmployment> getAllInstructorEmployments() {
        ArrayList<InstructorEmployment> allEmployments = new ArrayList<InstructorEmployment>();
        for (InstructorEmployment instructorEmployment : instructorEmployments) {
            allEmployments.add(instructorEmployment);
        }
        return allEmployments;
    }
    
    public ArrayList<InstructorEmployment> getAllEmployedInstructorEmployments() {
        ArrayList<InstructorEmployment> allEmployments = new ArrayList<InstructorEmployment>();
        for (InstructorEmployment instructorEmployment : instructorEmployments) {
            if (instructorEmployment.getEmploymentStatus().equals("employed")) {
                allEmployments.add(instructorEmployment);
            }
        }
        return allEmployments;
    }
    
    public ArrayList<InstructorEmployment> searchByInstructorName(String instructorName) {
        ArrayList<InstructorEmployment> instructorEmploymentsWithName = new ArrayList<InstructorEmployment>();
        for (InstructorEmployment instructorEmployment : instructorEmployments) {
            if (instructorEmployment.getInstructor().getName().equals(instructorName)) {
                instructorEmploymentsWithName.add(instructorEmployment);
            }
        }
        return instructorEmploymentsWithName;
    }
    
    public InstructorEmployment searchByInstructorId(String instructorId) throws InstructorIdNotFoundException {
        for (InstructorEmployment instructorEmployment : instructorEmployments) {
            if (instructorEmployment.instructorIdEquals(instructorId)) {
                return instructorEmployment;
            }
        }
        throw new InstructorIdNotFoundException(instructorId);
    }
    
    public ArrayList<InstructorEmployment> searchByEmploymentDate(LocalDate employmentDate) {
        ArrayList<InstructorEmployment> instructorEmploymentsWithDate = new ArrayList<InstructorEmployment>();
        for (InstructorEmployment instructorEmployment : instructorEmployments) {
            if (instructorEmployment.getEmploymentDate().equals(employmentDate)) {
                instructorEmploymentsWithDate.add(instructorEmployment);
            }
        }
        return instructorEmploymentsWithDate;
    }
    
    public ArrayList<InstructorEmployment> searchByEmploymentDateRange(LocalDate employmentDateStart, LocalDate employmentDateEnd) {
        LocalDate rangeStart = null;
        LocalDate rangeEnd = null;
        
        if (employmentDateStart.compareTo(employmentDateEnd) > 0) {
            rangeStart = employmentDateEnd;
            rangeEnd = employmentDateStart;
        } else {
            rangeStart = employmentDateStart;
            rangeEnd = employmentDateEnd;
        }
        
        ArrayList<InstructorEmployment> instructorEmploymentsWithDateRange = new ArrayList<InstructorEmployment>();
        for (InstructorEmployment instructorEmployment : instructorEmployments) {
            if (instructorEmployment.getEmploymentDate().compareTo(rangeStart) >= 0
                    && instructorEmployment.getEmploymentDate().compareTo(rangeEnd) <= 0) {
                instructorEmploymentsWithDateRange.add(instructorEmployment);
            }
        }
        return instructorEmploymentsWithDateRange;
    }
    
    public ArrayList<InstructorEmployment> searchByEmploymentDateMonth(int employmentMonth, int employmentYear) {
        ArrayList<InstructorEmployment> instructorEmploymentsWithDateMonth = new ArrayList<InstructorEmployment>();
        for (InstructorEmployment instructorEmployment : instructorEmployments) {
            if (instructorEmployment.getEmploymentDate().getMonthValue() == employmentMonth) {
                if (instructorEmployment.getEmploymentDate().getYear() == employmentYear) {
                    instructorEmploymentsWithDateMonth.add(instructorEmployment);
                }
            }
        }
        return instructorEmploymentsWithDateMonth;
    }
    
    public ArrayList<InstructorEmployment> searchByEmploymentDateYear(int employmentYear) {
        ArrayList<InstructorEmployment> instructorEmploymentsWithDateYear = new ArrayList<InstructorEmployment>();
        for (InstructorEmployment instructorEmployment : instructorEmployments) {
            if (instructorEmployment.getEmploymentDate().getYear() == employmentYear) {
                instructorEmploymentsWithDateYear.add(instructorEmployment);
            }
        }
        return instructorEmploymentsWithDateYear;
    }
    
    public String allToString() {
        String asString = "";
        for (InstructorEmployment instructorEmployment : instructorEmployments) {
            asString += instructorEmployment.toString();
        }
        return asString;
    }
    
    
    public boolean containsInstructorId(String testInstructorId) {
        for (InstructorEmployment instructorEmployment : instructorEmployments) {
            if (instructorEmployment.instructorIdEquals(testInstructorId)) {
                return true;
            }
        }
        return false;
    }
}
