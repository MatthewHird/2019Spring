package ca.viu.csci331.instruction.model;

import java.util.ArrayList;
import java.util.List;

public class Seminar {
    private String semniarId;
    private Course course;
    private int capacity;
    private Instructor instructor;
    private List<Schedule> schedules;
    
    public Seminar(String semniarId, Course course, int capacity, Instructor instructor) {
        this.semniarId = semniarId;
        this.course = course;
        this.capacity = capacity;
        this.instructor = instructor;
        schedules = new ArrayList<Schedule>();
    }
    
    public String getSeminarId() {
        return semniarId;
    }
    
    public Course getCourse() {
        return course;
    }
    
    public Instructor getInstructor() {
        return instructor;
    }
    
    public void addValidSchedule(Schedule addedSchedule) {
        if (!containsSchedule(addedSchedule)) {
            if (!containsOverlappingSchedule(addedSchedule)) {
                schedules.add(addedSchedule);
            } else {
                System.out.print("Could not add schedule: A schedule exists in the schedules list \n"
                        + "    with the same location and an overlapping time block\n\n");
            }
        } else {
            System.out.print("Could not add schedule: Identical schedule is already in schedules list\n\n");
        }
    }
    
    public void removeSchedule(Schedule removedSchedule) {
        schedules.removeIf(s -> (s.equals(removedSchedule)));
    }
    
    public void show() {
        System.out.printf("Seminar ID: %s\nCourse Name: %s\nCourse Number: %8s\nCapacity: %3d\nInstructor: %s\n"
                + "Schedule:\n", semniarId, course.getName(), course.getCourseNumber(), capacity, instructor.getName());
        
        for (Schedule schedule : schedules) {
            System.out.printf("    %9s, %02d:%02d Duration: %3d minutes Location: %s\n", schedule.getDay(),
                    schedule.getHour(), schedule.getMinute(), schedule.getDuration(), schedule.getLocation());
        }
        
        System.out.print("\n");
    }
    
    private boolean containsSchedule(Schedule other) {
        for (Schedule schedule : schedules) {
            if (schedule.equals(other)) {
                return true;
            }
        }
        return false;
    }
    
    private boolean containsOverlappingSchedule(Schedule other) {
        for (Schedule schedule : schedules) {
            if (schedule.equals(other)) {
                return true;
            }
        }
        return false;
    }
}