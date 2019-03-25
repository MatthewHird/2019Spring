package ca.viu.csci331.instruction.services.administration;

import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

import ca.viu.csci331.instruction.exception.DuplicateScheduleException;
import ca.viu.csci331.instruction.exception.ScheduleIdNotFoundException;
import ca.viu.csci331.instruction.exception.ScheduleNotFoundException;
import ca.viu.csci331.instruction.model.Schedule;

/**
 * Contains and manages Schedule objects in a list.
 * 
 * @author Matthew Hird
 * @date Mar. 25, 2019
 */
public class ScheduleList {
    private List<Schedule> scheduleList;
    
    public ScheduleList() {
        scheduleList = new ArrayList<Schedule>();
    }
    
    /**
     * @return  Number of schedules currently in ScheduleList.
     */
    public int getScheduleCount() {
        return scheduleList.size();
    }
    
    /**
     * @param addedSchedule     Schedule to add to ScheduleList.
     * @throws DuplicateScheduleException   If addedSchedule already is in
     *                                      the ScheduleList.
     */
    public void add(Schedule addedSchedule) throws DuplicateScheduleException {
        if (containsOverlap(addedSchedule)) {
            throw new DuplicateScheduleException(addedSchedule);
        }
        scheduleList.add(addedSchedule);
    }
    
    /**
     * @param removedSchedule   Schedule to remove from ScheduleList.
     * @return  Removed Schedule.
     * @throws ScheduleNotFoundException    If Schedule is not in ScheduleList.
     */
    public Schedule remove(Schedule removedSchedule) throws ScheduleNotFoundException {
        int removeIndex = -1;
        for (int i = 0; i < scheduleList.size(); i++) {
            if (scheduleList.get(i).doesOverlap(removedSchedule)) {
                removeIndex = i;
                break;
            }
        }
        if (removeIndex == -1) {
            throw new ScheduleNotFoundException(removedSchedule);
        }
        return scheduleList.remove(removeIndex);
    }
    
    /**
     * @param removedScheduleId     ID of Schedule to fromove from ScheduleList.
     * @return  Removed Schedule.
     * @throws ScheduleIdNotFoundException  If Schedule is not in ScheduleList.
     */
    public Schedule remove(String removedScheduleId) throws ScheduleIdNotFoundException {
        int removeIndex = -1;
        for (int i = 0; i < scheduleList.size(); i++) {
            if (scheduleList.get(i).scheduleIdEquals(removedScheduleId)) {
                removeIndex = i;
                break;
            }
        }
        if (removeIndex == -1) {
            throw new ScheduleIdNotFoundException(removedScheduleId);
        }
        return scheduleList.remove(removeIndex);
    }
    
    /**
     * Find a Schedule based on its ID.
     * 
     * @param scheduleId    ID of Schedule being searched for.
     * @return  Schedule with ID of scheduleId.
     * @throws ScheduleIdNotFoundException  If Schedule with ID scheduleId is
     *                                      not in ScheduleList.
     */
    public Schedule searchByScheduleId(String scheduleId) throws ScheduleIdNotFoundException {
        for (Schedule schedule : scheduleList) {
            if (schedule.scheduleIdEquals(scheduleId)) {
                return schedule;
            }
        }
        throw new ScheduleIdNotFoundException(scheduleId);
    }
    
    /**
     * Find all schedules for a particular room.
     * 
     * @param buildingNumber    ID of building being searched for.
     * @param roomNumber        ID of room within building being searched for.
     * @return  A list of Schedule objects for the specified room.
     */
    public ArrayList<Schedule> searchByLocation(String buildingNumber, String roomNumber) {
        ArrayList<Schedule> schedulesAtLocation = new ArrayList<Schedule>();
        for (Schedule schedule : scheduleList) {
            if (schedule.getLocation().equalBuildNumRoomNum(buildingNumber, roomNumber) ) {
                schedulesAtLocation.add(schedule);
            }
        }
        return schedulesAtLocation;
    }

    /**
     * Find all schedules with a seat capacity greater than or equal to the 
     * specified minimum capacity.
     * 
     * @param minRoomCapacity   Minimum seat capacity.
     * @return  All schedules for rooms with capacity >= minRoomCapacity.
     */
    public ArrayList<Schedule> searchByMinRoomCapacity(int minRoomCapacity) {
        ArrayList<Schedule> schedulesWithCapacity = new ArrayList<Schedule>();
        for (Schedule schedule : scheduleList) {
            if (schedule.getLocation().getCapacity() >= minRoomCapacity) {
                schedulesWithCapacity.add(schedule);
            }
        }
        return schedulesWithCapacity;
    }
    
    /**
     * Find all schedules within a specified time block.
     * 
     * @param dayOfWeek         The day of the week for the schedule block.
     *      Must be one of the folowing values: "sun", "sunday", "mon", 
     * "monday", "tue", "tues", "tuesday", "wed", "wednesday", "thu", "thur", 
     * "thurs", "thursday", "fri", "friday", "sat", or "saturday".
     * 
     * @param startTime         The start time of the schedule block.
     * @param durationMinutes   The duration of the schedule block in minutes.
     * @return  The list of all schedules within the specified time block.
     */
    public ArrayList<Schedule> searchByTimeBlock(String dayOfWeek, LocalTime startTime, int durationMinutes) {
        ArrayList<Schedule> schedulesWithinTimeblock = new ArrayList<Schedule>();
        for (Schedule schedule : scheduleList) {
            if (schedule.dayEquals(dayOfWeek) 
                    && schedule.getStartTime().equals(startTime) 
                    && schedule.getDuration() == durationMinutes) {
                schedulesWithinTimeblock.add(schedule);
            }
        }
        return schedulesWithinTimeblock;
    }
    
    /**
     * @param dayOfWeek         The day of the week for the schedule block.
     *      Must be one of the folowing values: "sun", "sunday", "mon", 
     * "monday", "tue", "tues", "tuesday", "wed", "wednesday", "thu", "thur", 
     * "thurs", "thursday", "fri", "friday", "sat", or "saturday".
     * 
     * @param startTime         The start time of the schedule block.
     * @param durationMinutes   The duration of the schedule block in minutes.
     * @param minRoomCapacity   Minimum seat capacity.
     * @return  The list of all schedules within the specified time block that
     *          have a minimum seat capacity of minSeatCapacity.
     */
    public ArrayList<Schedule> searchByTimeBlockAndCapacity(String dayOfWeek, 
            LocalTime startTime, int durationMinutes, int minRoomCapacity) {
        ArrayList<Schedule> schedulesWithinTimeblock = new ArrayList<Schedule>();
        for (Schedule schedule : scheduleList) {
            if (schedule.dayEquals(dayOfWeek) 
                    && schedule.getStartTime().equals(startTime) 
                    && schedule.getDuration() == durationMinutes
                    && schedule.getLocation().getCapacity() >= minRoomCapacity) {
                schedulesWithinTimeblock.add(schedule);
            }
        }
        return schedulesWithinTimeblock;
    }
    
    /**
     * @return  A list of all schedule stored in the ScheduleList.
     */
    public ArrayList<Schedule> getAllSchedules() {
        ArrayList<Schedule> allSchedules = new ArrayList<Schedule>();
        for (Schedule schedule : scheduleList) {
            allSchedules.add(schedule);
        }
        return allSchedules;
    }
    
    /**
     * Checks if ScheduleList contains Schedule with specified ID.
     * 
     * @param scheduleId    ID being checked.
     * @return  true if Schedule with ID of scheduleId is in ScheduleList.
     *          Otherwise, false.
     */
    public boolean containsScheduleId(String scheduleId) {
        for (Schedule schedule : scheduleList) {
            if (schedule.scheduleIdEquals(scheduleId)) {
                return true;
            }
        }
        return false;
    }
    
    /**
     * Checks if specified Schedule overlaps with a Schedule already in the
     * ScheduleList.
     * 
     * @param testSchedule  Schedule being compared.
     * @return  true if testSchedule is in the same location on the same day
     *          of the week, and the timeblock (startTime to startTime 
     *          + duration) overlaps with a Schedule already in ScheduleList.
     */
    public boolean containsOverlap(Schedule testSchedule) {
        for (Schedule schedule : scheduleList) {
            if (schedule.doesOverlap(testSchedule)) {
                return true;
            }
        }
        return false;
    }
}
