package ca.viu.csci331.instruction.model;

import java.util.ArrayList;
import java.util.List;

public class Enrollments {
    private List<Enrollment> enrollments;
    private int capacity;
    private int enrollmentCount;
    
    public Enrollments(int capacity) {
        this.capacity = capacity;
        enrollmentCount = 0;
        enrollments = new ArrayList<Enrollment>();
    }
    
    public void enroll(Enrollment addedEnrollment) {
        if (enrollmentCount < capacity) {
            enrollments.add(addedEnrollment);
            enrollmentCount++;
        } else {
            System.out.print("Could not add enrollment: Enrollment list at capacity\n\n");
        }
        
    }
    
    public void cancel(Enrollment cancelledEnrollment) {
        enrollments.removeIf(s -> (s.equals(cancelledEnrollment)));
        enrollmentCount = enrollments.size();
    }
    
    public void showAll() {
        System.out.print("\n~~~~~~~~~~~~~~~~~~~~\nList of All Enrollments\n~~~~~~~~~~~~~~~~~~~~\n\n");
        for (Enrollment enrollment : enrollments) {
            enrollment.show();
        }
        System.out.print("~~~~~~~~~~~~~~~~~~~~\n\n");
    }
}
