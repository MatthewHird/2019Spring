package ca.viu.csci331.instruction.services.administration;

import java.util.ArrayList;
import java.util.List;

import ca.viu.csci331.instruction.exception.DuplicateSeminarException;
import ca.viu.csci331.instruction.exception.SeminarIdNotFoundException;
import ca.viu.csci331.instruction.model.Course;
import ca.viu.csci331.instruction.model.Instructor;
import ca.viu.csci331.instruction.model.Seminar;

public class SeminarList {
    private List<Seminar> seminarList;
    
    public SeminarList() {
        seminarList = new ArrayList<Seminar>();
    }
    
    public int getSeminarCount() {
        return seminarList.size();
    }
    
    public void add(Seminar addedSeminar) throws DuplicateSeminarException {
        if (containsSeminarId(addedSeminar)) {
            throw new DuplicateSeminarException(addedSeminar);
        }
        seminarList.add(addedSeminar);
    }
    
    public void add(String semniarId, Course course, int capacity, 
            Instructor instructor) throws DuplicateSeminarException {
        if (containsSeminarId(semniarId)) {
            throw new DuplicateSeminarException(semniarId);
        }
        seminarList.add(new Seminar(semniarId, course, capacity, instructor));
    }
    
    public Seminar remove(Seminar removedSeminar) throws SeminarIdNotFoundException {
        int removeIndex = -1;
        for (int i = 0; i < seminarList.size(); i++) {
            if (seminarList.get(i).seminarIdEquals(removedSeminar)) {
                removeIndex = i;
                break;
            }
        }
        if (removeIndex == -1) {
            throw new SeminarIdNotFoundException(removedSeminar.getSeminarId());
        }
        return seminarList.remove(removeIndex);
    }
    
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
    
    public Seminar searchBySeminarId(String seminarId) throws SeminarIdNotFoundException {
        for (Seminar seminar : seminarList) {
            if (seminar.seminarIdEquals(seminarId) ) {
                return seminar;
            }
        }
        throw new SeminarIdNotFoundException(seminarId);
    }
    
    public boolean containsSeminarId(Seminar testSeminar) {
        for (Seminar seminar : seminarList) {
            if (seminar.seminarIdEquals(testSeminar)) {
                return true;
            }
        }
        return false;
    }
    
    public boolean containsSeminarId(String testSeminarId) {
        for (Seminar seminar : seminarList) {
            if (seminar.seminarIdEquals(testSeminarId)) {
                return true;
            }
        }
        return false;
    }
}
