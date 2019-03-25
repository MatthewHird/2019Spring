package ca.viu.csci331.instruction.services.administration;

import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

import ca.viu.csci331.instruction.exception.DuplicateScheduleException;
import ca.viu.csci331.instruction.exception.ScheduleIdNotFoundException;
import ca.viu.csci331.instruction.exception.ScheduleNotFoundException;
import ca.viu.csci331.instruction.model.BuildingRoom;
import ca.viu.csci331.instruction.model.Schedule;

public class ScheduleList {
    private List<Schedule> scheduleList;
    
    public ScheduleList() {
        scheduleList = new ArrayList<Schedule>();
    }
    
    public int getScheduleCount() {
        return scheduleList.size();
    }
    
    public void add(Schedule addedSchedule) throws DuplicateScheduleException {
        if (containsOverlap(addedSchedule)) {
            throw new DuplicateScheduleException(addedSchedule);
        }
        scheduleList.add(addedSchedule);
    }
    
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
    
    public Schedule searchByScheduleId(String scheduleId) throws ScheduleIdNotFoundException {
        for (Schedule schedule : scheduleList) {
            if (schedule.scheduleIdEquals(scheduleId)) {
                return schedule;
            }
        }
        throw new ScheduleIdNotFoundException(scheduleId);
    }
    
    public ArrayList<Schedule> searchByLocation(String buildingNumber, String roomNumber) {
        ArrayList<Schedule> schedulesAtLocation = new ArrayList<Schedule>();
        for (Schedule schedule : scheduleList) {
            if (schedule.getLocation().equalBuildNumRoomNum(buildingNumber, roomNumber) ) {
                schedulesAtLocation.add(schedule);
            }
        }
        return schedulesAtLocation;
    }

    public ArrayList<Schedule> searchByMinRoomCapacity(int minRoomCapacity) {
        ArrayList<Schedule> schedulesWithCapacity = new ArrayList<Schedule>();
        for (Schedule schedule : scheduleList) {
            if (schedule.getLocation().getCapacity() >= minRoomCapacity) {
                schedulesWithCapacity.add(schedule);
            }
        }
        return schedulesWithCapacity;
    }
    
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
    
    public boolean containsScheduleId(String scheduleId) {
        for (Schedule schedule : scheduleList) {
            if (schedule.scheduleIdEquals(scheduleId)) {
                return true;
            }
        }
        return false;
    }
    
    public boolean containsOverlap(Schedule testSchedule) {
        for (Schedule schedule : scheduleList) {
            if (schedule.doesOverlap(testSchedule)) {
                return true;
            }
        }
        return false;
    }
}
