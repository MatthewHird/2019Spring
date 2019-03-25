package ca.viu.csci331.instruction.services.administration;

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
    
    public void add(String instructorName, String instructorId, String instructorEmail) throws DuplicateInstructorEmploymentException {
        if (containsInstructorId(instructorId)) {
            throw new DuplicateInstructorEmploymentException(instructorId);
        } 
        instructorEmployments.add(new InstructorEmployment(
                new Instructor(instructorName, instructorId, instructorEmail)));
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
    
    public boolean containsInstructorId(String testInstructorId) {
        for (InstructorEmployment instructorEmployment : instructorEmployments) {
            if (instructorEmployment.instructorIdEquals(testInstructorId)) {
                return true;
            }
        }
        return false;
    }
    
//    public String allToString() {
//        String asString = "";
//        for (InstructorEmployment instructorEmployment : instructorEmployments) {
//            asString += instructorEmployment.toString();
//        }
//        return asString;
//    }
}
