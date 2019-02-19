package ca.viu.csci331.instruction.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Business classes that manages a list of <Seminar> objects in a university system.
 * @author Matthew Hird
 * @date Feb. 12, 2019
 */
public class Seminars {
    private List<Seminar> seminars;
    private int capacity;
    private int seminarCount;
    
    /**
     * Primary constructor
     * @param capacity Maximum number of <Seminar> objects this instance will store and manage.
     */
    public Seminars(int capacity) {
        this.capacity = capacity;
        seminarCount = 0;
        seminars = new ArrayList<Seminar>();
    }
    
    public void add(Seminar addedSeminar) {
        if (seminarCount < capacity) {
            if (!containsSeminar(addedSeminar)) {
                seminars.add(addedSeminar);
                seminarCount++;
            } else {
                System.out.print("Could not add seminar: Seminar with same ID already in seminars list\n\n");
            }
        } else {
            System.out.print("Could not add seminar: Seminars list at capacity\n\n");
        }
    }
    
    public void cancel(Seminar cancelledSeminar) {
        seminars.removeIf(s -> (s.getSeminarId().equals(cancelledSeminar.getSeminarId())));
        seminarCount = seminars.size();
    }
    
    public void showAll() {
        System.out.print("\n~~~~~~~~~~~~~~~~~~~~\nList of All Seminars\n~~~~~~~~~~~~~~~~~~~~\n\n");
        for (Seminar seminar : seminars) {
            seminar.show();
        }
        System.out.print("~~~~~~~~~~~~~~~~~~~~\n\n");
    }
    
    public Seminar searchById(String seminarId) {
        return new Seminar("SEMINARNOTFOUND", new Course("", "", 0, ""), 0, new Instructor("", "", ""));
    }
    
    private boolean containsSeminar(Seminar other) {
        for (Seminar seminar : seminars) {
            if (seminar.getSeminarId().equals(other.getSeminarId())) {
                return true;
            }
        }
        return false;
    }
}
