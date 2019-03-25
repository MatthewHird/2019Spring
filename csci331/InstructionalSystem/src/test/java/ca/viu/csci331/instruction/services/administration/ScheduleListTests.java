package ca.viu.csci331.instruction.services.administration;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Collections;

import org.junit.Before;
import org.junit.Test;

import ca.viu.csci331.instruction.exception.DuplicateScheduleException;
import ca.viu.csci331.instruction.exception.ScheduleIdNotFoundException;
import ca.viu.csci331.instruction.exception.ScheduleNotFoundException;
import ca.viu.csci331.instruction.model.BuildingRoom;
import ca.viu.csci331.instruction.model.Schedule;

public class ScheduleListTests {
    private ScheduleList scheduleList;
    
    @Before
    public void prepareScheduleList() {
        scheduleList = new ScheduleList();
        
        try {
            scheduleList.add(new Schedule("S001", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B001", "R001", 20)));
            scheduleList.add(new Schedule("S002", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B001", "R002", 10)));
            scheduleList.add(new Schedule("S003", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B002", "R003", 100)));
            scheduleList.add(new Schedule("S004", "tues", LocalTime.of(13, 30), 120, new BuildingRoom("B002", "R003", 100)));
        } catch (DuplicateScheduleException e) {
            e.printStackTrace();
        }
    }
    
    @Test
    public void testAdd() throws DuplicateScheduleException {
        ArrayList<Schedule> expected = new ArrayList<Schedule>();
        expected.add(new Schedule("S001", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B001", "R001", 20)));
        expected.add(new Schedule("S002", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B001", "R002", 10)));
        expected.add(new Schedule("S003", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B002", "R003", 100)));
        expected.add(new Schedule("S004", "tues", LocalTime.of(13, 30), 120, new BuildingRoom("B002", "R003", 100)));
        expected.add(new Schedule("S005", "tues", LocalTime.of(13, 30), 120, new BuildingRoom("B003", "R003", 100)));
        
        scheduleList.add(new Schedule("S005", "tues", LocalTime.of(13, 30), 120, new BuildingRoom("B003", "R003", 100)));
        
        assertTrue(compareScheduleArrayList(expected, scheduleList.getAllSchedules()));
    }
    
    @Test
    public void testRemoveParamSchedule() throws ScheduleNotFoundException {
        ArrayList<Schedule> expected = new ArrayList<Schedule>();
        expected.add(new Schedule("S001", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B001", "R001", 20)));
        expected.add(new Schedule("S002", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B001", "R002", 10)));
        expected.add(new Schedule("S004", "tues", LocalTime.of(13, 30), 120, new BuildingRoom("B002", "R003", 100)));
        
        scheduleList.remove(new Schedule("S003", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B002", "R003", 100)));

        assertTrue(compareScheduleArrayList(expected, scheduleList.getAllSchedules()));
    }
    
    @Test
    public void testRemoveParamString() throws ScheduleIdNotFoundException {
        ArrayList<Schedule> expected = new ArrayList<Schedule>();
        expected.add(new Schedule("S001", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B001", "R001", 20)));
        expected.add(new Schedule("S002", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B001", "R002", 10)));
        expected.add(new Schedule("S004", "tues", LocalTime.of(13, 30), 120, new BuildingRoom("B002", "R003", 100)));
        
        scheduleList.remove("S003");

        assertTrue(compareScheduleArrayList(expected, scheduleList.getAllSchedules()));
    }
    
    @Test
    public void testSearchByScheduleId() throws ScheduleIdNotFoundException {
        Schedule expected = new Schedule("S002", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B001", "R002", 10));
        
        assertTrue(expected.scheduleIdEquals(scheduleList.searchByScheduleId("S002")));
    }
    
    @Test
    public void testSearchByLocation() {
        ArrayList<Schedule> expected = new ArrayList<Schedule>();
        expected.add(new Schedule("S003", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B002", "R003", 100)));
        expected.add(new Schedule("S004", "tues", LocalTime.of(13, 30), 120, new BuildingRoom("B002", "R003", 100)));
        
        assertTrue(compareScheduleArrayList(expected, scheduleList.searchByLocation("B002", "R003")));
    }
    
    @Test
    public void testSearchByMinRoomCapacity() {
        ArrayList<Schedule> expected = new ArrayList<Schedule>();
        expected.add(new Schedule("S001", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B001", "R001", 20)));
        expected.add(new Schedule("S003", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B002", "R003", 100)));
        expected.add(new Schedule("S004", "tues", LocalTime.of(13, 30), 120, new BuildingRoom("B002", "R003", 100)));
        
        assertTrue(compareScheduleArrayList(expected, scheduleList.searchByMinRoomCapacity(15)));
    }
    
    @Test
    public void testSearchByTimeBlock() {
        ArrayList<Schedule> expected = new ArrayList<Schedule>();
        expected.add(new Schedule("S001", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B001", "R001", 20)));
        expected.add(new Schedule("S002", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B001", "R002", 10)));
        expected.add(new Schedule("S003", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B002", "R003", 100)));
        
        assertTrue(compareScheduleArrayList(expected, scheduleList.searchByTimeBlock("mon", LocalTime.of(10, 0), 60)));
    }
    
    @Test
    public void testSearchByTimeBlockAndCapacity() {
        ArrayList<Schedule> expected = new ArrayList<Schedule>();
        expected.add(new Schedule("S001", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B001", "R001", 20)));
        expected.add(new Schedule("S003", "mon", LocalTime.of(10, 0), 60, new BuildingRoom("B002", "R003", 100)));
        
        assertTrue(compareScheduleArrayList(expected, scheduleList.searchByTimeBlockAndCapacity("mon", LocalTime.of(10, 0), 60, 15)));
    }
    
    @Test
    public void testContainsScheduleId() {
        assertTrue(scheduleList.containsScheduleId("S002"));
        assertFalse(scheduleList.containsScheduleId("S008"));
    }
    
    @Test
    public void testContainsOverlap() {
        assertTrue(scheduleList.containsOverlap(new Schedule("S003", "mon", LocalTime.of(8, 0), 125, new BuildingRoom("B002", "R003", 100))));
        assertFalse(scheduleList.containsOverlap(new Schedule("S003", "mon", LocalTime.of(8, 0), 120, new BuildingRoom("B002", "R003", 100))));
    }
    
    private boolean compareScheduleArrayList(ArrayList<Schedule> list1, ArrayList<Schedule> list2) { 
        if (list1.size() == list2.size()) {
            Collections.sort(list1);
            Collections.sort(list2);
            for (int i = 0; i < list1.size(); i++) {
                if (!list1.get(i).scheduleIdEquals(list2.get(i))) {
                    return false;
                }
            }
        } else {
            return false;
        }
        return true;
    }
}
