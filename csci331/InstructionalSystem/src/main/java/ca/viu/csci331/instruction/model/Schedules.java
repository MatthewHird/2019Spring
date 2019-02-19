package ca.viu.csci331.instruction.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Manages a list of available <Schedule> objects in a university system.
 * @author Matthew Hird
 * @date Feb. 12, 2019
 */
public class Schedules {
    private List<Schedule> schedules;
    private int capacity;
    private int scheduleCount;
    
    /**
     * Primary constructor
     * @param capacity Maximim number of <Schedule> objects this instance will store and manage.
     */
    public Schedules(int capacity) {
        this.capacity = capacity;
        scheduleCount = 0;
        schedules = new ArrayList<Schedule>();
    }
    
    public void add(Schedule addedSchedule) {
        if (scheduleCount < capacity) {
            if (!containsSchedule(addedSchedule)) {
                if (!containsOverlappingSchedule(addedSchedule)) {
                    schedules.add(addedSchedule);
                    scheduleCount++;
                } else {
                    System.out.print("Could not add schedule: A schedule exists in the schedules list \n"
                            + "    with the same location and an overlapping time block\n\n");
                }
            } else {
                System.out.print("Could not add schedule: Identical schedule is already in schedules list\n\n");
            }
        } else {
            System.out.print("Could not add schedule: Schedules list at capacity\n\n");
        }
    }
    
    public void remove(Schedule removedSchedule) {
        schedules.removeIf(s -> (s.equals(removedSchedule)));
        scheduleCount = schedules.size();
    }
    
    public void showAll() {
        System.out.print("\n~~~~~~~~~~~~~~~~~~~~\nList of All Schedules\n~~~~~~~~~~~~~~~~~~~~\n\n");
        for (Schedule schedule : schedules) {
            schedule.show();
        }
        System.out.print("~~~~~~~~~~~~~~~~~~~~\n\n");
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
