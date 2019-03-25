package ca.viu.csci331.instruction.services.administration;

import java.util.ArrayList;
import java.util.List;

import ca.viu.csci331.instruction.exception.DuplicateInstructorEmploymentException;
import ca.viu.csci331.instruction.exception.InstructorIdNotFoundException;
import ca.viu.csci331.instruction.model.Instructor;
import ca.viu.csci331.instruction.model.InstructorEmployment;

/**
 * Contains and manages InstructorEmployment objects in a list.
 * 
 * @author Matthew Hird
 * @date Mar. 25, 2019
 */
public class InstructorEmploymentList {
    private List<InstructorEmployment> instructorEmployments;
    
    public InstructorEmploymentList() {
        instructorEmployments = new ArrayList<InstructorEmployment>();
    }
    
    /**
     * @return  Number of InstructorEmployment objects in InstructorEmploymentList.
     */
    public int getInstructorCount() {
        return instructorEmployments.size();
    }
    
    /**
     * Adds new InstructorEmployment to list, with employmentStatus = "employed"
     * and employmentDate = LocalDate.now().
     * 
     * @param instructorName    Name of Instructor.
     * @param instructorId      ID of Instructor.
     * @param instructorEmail   Email of Instructor.
     * @throws DuplicateInstructorEmploymentException   If InstructorEmployment 
     *                          with ID instructorId already exists in list.
     */
    public void add(String instructorName, String instructorId, String instructorEmail) throws DuplicateInstructorEmploymentException {
        if (containsInstructorId(instructorId)) {
            throw new DuplicateInstructorEmploymentException(instructorId);
        } 
        instructorEmployments.add(new InstructorEmployment(
                new Instructor(instructorName, instructorId, instructorEmail)));
    }
    
    /**
     * @param removedInstructorId   ID of Instructor.
     * @return  The removed InstructorEmployment.
     * @throws InstructorIdNotFoundException    If InstructorEmployment with ID
     *                                       removedInstructorId is not in list.
     */
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
    
    /**
     * @return  List of all InstructorEmployments in InstructorEmploymentList.
     */
    public ArrayList<InstructorEmployment> getAllInstructorEmployments() {
        ArrayList<InstructorEmployment> allEmployments = new ArrayList<InstructorEmployment>();
        for (InstructorEmployment instructorEmployment : instructorEmployments) {
            allEmployments.add(instructorEmployment);
        }
        return allEmployments;
    }
    
    /**
     * @return  List of all InstructorEmployments in InstructorEmploymentList 
     *          with employmentStatus == "employed".
     */
    public ArrayList<InstructorEmployment> getAllEmployedInstructorEmployments() {
        ArrayList<InstructorEmployment> allEmployments = new ArrayList<InstructorEmployment>();
        for (InstructorEmployment instructorEmployment : instructorEmployments) {
            if (instructorEmployment.getEmploymentStatus().equals("employed")) {
                allEmployments.add(instructorEmployment);
            }
        }
        return allEmployments;
    }
    
    /**
     * Find all InstructorEmployments with specified instructor name.
     * 
     * @param instructorName    Instructor name.
     * @return  All InstructorEmployments with instructor name = instructorName.
     */
    public ArrayList<InstructorEmployment> searchByInstructorName(String instructorName) {
        ArrayList<InstructorEmployment> instructorEmploymentsWithName = new ArrayList<InstructorEmployment>();
        for (InstructorEmployment instructorEmployment : instructorEmployments) {
            if (instructorEmployment.getInstructor().getName().equals(instructorName)) {
                instructorEmploymentsWithName.add(instructorEmployment);
            }
        }
        return instructorEmploymentsWithName;
    }
    
    /**
     * Find the InstructorEmployment with specified instructor ID.
     * 
     * @param instructorId  Instructor ID. 
     * @return  InstructorEmployment with instructor ID = instructorId.
     * @throws InstructorIdNotFoundException    If InstructorEmployment with
     *                      instructor ID == instructorId is not in list.
     */
    public InstructorEmployment searchByInstructorId(String instructorId) throws InstructorIdNotFoundException {
        for (InstructorEmployment instructorEmployment : instructorEmployments) {
            if (instructorEmployment.instructorIdEquals(instructorId)) {
                return instructorEmployment;
            }
        }
        throw new InstructorIdNotFoundException(instructorId);
    }
    
    /**
     * Checks for existence of InstructorEmployment with specified instructor ID.
     * 
     * @param testInstructorId  Instructor ID.
     * @return  true if InstructorEmployment with instructor ID 
     *          == testInstructorId is in InstructorEmploymentList. 
     *          Otherwise, false.
     */
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
