package ca.viu.csci331.instruction.model;

import java.util.ArrayList;
import java.util.List;

public class Instructors {
    private List<Instructor> instructors;
    private int capacity;
    private int instructorCount;
    
    public Instructors(int capacity) {
        this.capacity = capacity;
        instructorCount = 0;
        instructors = new ArrayList<Instructor>();
    }
    
    public void hire(Instructor hiredInstructor) {
        if (instructorCount < capacity) {
            if (!containsId(hiredInstructor.getInstructorId())) {
                instructors.add(hiredInstructor);
                instructorCount++;
            } else {
                System.out.print("Could not hire instructor: Instructor with same ID already in system\n\n");
            }
        } else {
            System.out.print("Could not hire instructor: Instructors list at capacity\n\n");
        }
    }
    
    public void terminate(Instructor terminatedInstructor) {
        instructors.removeIf(s -> (s.getInstructorId().equals(terminatedInstructor.getInstructorId())));
        instructorCount = instructors.size();
    }
    
    public void showAll() {
        System.out.print("\n~~~~~~~~~~~~~~~~~~~~\nList of All Instructors\n~~~~~~~~~~~~~~~~~~~~\n\n");
        for (Instructor instructor: instructors) {
            instructor.show();
        }
        System.out.print("~~~~~~~~~~~~~~~~~~~~\n\n");
    }
    
    public Instructor searchByName(String instructorName) {
        for (Instructor instructor : instructors) {
            if (instructor.getName().equals(instructorName)) {
                return instructor;
            }
        }
        return new Instructor("INSTRUCTORNOTFOUND", "INSTRUCTORNOTFOUND", "");
    }
    
    public Instructor searchById(String instructorId) {
        for (Instructor instructor : instructors) {
            if (instructor.getInstructorId().equals(instructorId)) {
                return instructor;
            }
        }
        return new Instructor("INSTRUCTORNOTFOUND", "INSTRUCTORNOTFOUND", "");
    }
    
    private boolean containsId(String instructorId) {
        for (Instructor instructor : instructors) {
            if (instructor.getInstructorId().equals(instructorId)) {
                return true;
            }
        }
        return false;
    }
}
