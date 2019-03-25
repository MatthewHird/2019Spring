package ca.viu.csci331.instruction.services.administration;

import java.util.ArrayList;
import java.util.List;

import ca.viu.csci331.instruction.exception.DuplicateSeminarException;
import ca.viu.csci331.instruction.exception.SeminarIdNotFoundException;
import ca.viu.csci331.instruction.model.Course;
import ca.viu.csci331.instruction.model.Instructor;
import ca.viu.csci331.instruction.model.Seminar;

/**
 * Contains and manages Seminar objects in a list.
 * 
 * @author Matthew Hird
 * @date Mar. 25, 2019
 */
public class SeminarList {
    private List<Seminar> seminarList;
    
    public SeminarList() {
        seminarList = new ArrayList<Seminar>();
    }
    
    /**
     * @return Number of Seminar objects in SeminarList.
     */
    public int getSeminarCount() {
        return seminarList.size();
    }
    
    /**
     * Add new Seminar to list.
     * 
     * @param semniarId     Id of new Seminar.
     * @param course        Course the Seminar is a part of.
     * @param capacity      Number of available seats in Seminar.
     * @param instructor    Instructor teaching the Seminar.
     * @throws DuplicateSeminarException    If Seminar with the same seminarId
     *                                      value is already in the list.
     */
    public void add(String semniarId, Course course, int capacity, 
            Instructor instructor) throws DuplicateSeminarException {
        if (containsSeminarId(semniarId)) {
            throw new DuplicateSeminarException(semniarId);
        }
        seminarList.add(new Seminar(semniarId, course, capacity, instructor));
    }
    
    /**
     * Remove a Seminar from the list.
     * 
     * @param removedSeminarId  The seminarId of the Seminar being removed.
     * @return                  The removed Seminar.
     * @throws SeminarIdNotFoundException   If Seminar with seminarId is not
     *                                      in the SeminarList.
     */
    public Seminar remove(String removedSeminarId) throws SeminarIdNotFoundException {
        int removeIndex = -1;
        for (int i = 0; i < seminarList.size(); i++) {
            if (seminarList.get(i).seminarIdEquals(removedSeminarId)) {
                removeIndex = i;
                break;
            }
        }
        if (removeIndex == -1) {
            throw new SeminarIdNotFoundException(removedSeminarId);
        }
        return seminarList.remove(removeIndex);
    }
    
    /**
     * Find Seminar with the ID seminarId.
     * 
     * @param seminarId     ID of the Seminar.
     * @return              The Seminar with ID seminarId.
     * @throws SeminarIdNotFoundException   If Seminar with seminarId is not
     *                                      in the SeminarList.
     */
    public Seminar searchBySeminarId(String seminarId) throws SeminarIdNotFoundException {
        for (Seminar seminar : seminarList) {
            if (seminar.seminarIdEquals(seminarId) ) {
                return seminar;
            }
        }
        throw new SeminarIdNotFoundException(seminarId);
    }
    
    /**
     * Get all Seminar ibjects in list.
     * 
     * @return  All Seminar objects in SeminarList
     */
    public ArrayList<Seminar> getAllSeminars() {
        ArrayList<Seminar> allSeminar = new ArrayList<Seminar>();
        for (Seminar seminar : seminarList) {
            allSeminar.add(seminar);
        }
        return allSeminar;
    }
    
    /**
     * Checks for the existance of a Seminar in the list.
     * 
     * @param testSeminarId     Id of Seminar being checked.
     * @return  true if Seminar with ID testSeminarId is in SeminarList;
     *          otherwise, false.
     */
    public boolean containsSeminarId(String testSeminarId) {
        for (Seminar seminar : seminarList) {
            if (seminar.seminarIdEquals(testSeminarId)) {
                return true;
            }
        }
        return false;
    }
}
