package ca.viu.csci331.instruction.services.administration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Collections;

import org.junit.Before;
import org.junit.Test;

import com.sun.org.apache.xalan.internal.lib.ExsltStrings;

import ca.viu.csci331.instruction.exception.BuildingRoomNotFoundException;
import ca.viu.csci331.instruction.exception.CourseNameNotFoundException;
import ca.viu.csci331.instruction.exception.CourseNumberNotFoundException;
import ca.viu.csci331.instruction.exception.DuplicateBuildingRoomException;
import ca.viu.csci331.instruction.exception.DuplicateCourseNameException;
import ca.viu.csci331.instruction.exception.DuplicateCourseNumberException;
import ca.viu.csci331.instruction.exception.DuplicateScheduleException;
import ca.viu.csci331.instruction.exception.DuplicateSeminarException;
import ca.viu.csci331.instruction.exception.InstructorIdNotFoundException;
import ca.viu.csci331.instruction.exception.InvalidBuildingNumberException;
import ca.viu.csci331.instruction.exception.InvalidCourseCreditsException;
import ca.viu.csci331.instruction.exception.InvalidCourseNameException;
import ca.viu.csci331.instruction.exception.InvalidCourseNumberException;
import ca.viu.csci331.instruction.exception.InvalidInstructorEmailException;
import ca.viu.csci331.instruction.exception.InvalidInstructorNameException;
import ca.viu.csci331.instruction.exception.InvalidRoomNumberException;
import ca.viu.csci331.instruction.exception.ScheduleIdNotFoundException;
import ca.viu.csci331.instruction.exception.ScheduleNotFoundException;
import ca.viu.csci331.instruction.exception.SeminarIdNotFoundException;
import ca.viu.csci331.instruction.exception.SeminarRoomCapacityConflictException;
import ca.viu.csci331.instruction.model.BuildingRoom;
import ca.viu.csci331.instruction.model.Course;
import ca.viu.csci331.instruction.model.Instructor;
import ca.viu.csci331.instruction.model.InstructorEmployment;
import ca.viu.csci331.instruction.model.Schedule;
import ca.viu.csci331.instruction.model.Seminar;

public class AdministrationServiceTests {
    private AdministrationService adminService;
    private ArrayList<String> instructorIdList;
    private ArrayList<String> scheduleIdList;
    
    @Before
    public void prepareAdminService() {
        adminService = new AdministrationService();
        instructorIdList = new ArrayList<String>();
        scheduleIdList = new ArrayList<String>();
        
        try {
            instructorIdList.add(adminService.hireNewInstructor("Instructor 1", "e1@mail.com"));
            instructorIdList.add(adminService.hireNewInstructor("Instructor 2", "e2@mail.com"));
            instructorIdList.add(adminService.hireNewInstructor("Instructor 3", "e3@mail.com"));
            instructorIdList.add(adminService.hireNewInstructor("Instructor 4", "e4@mail.com"));
        } catch (InvalidInstructorNameException | InvalidInstructorEmailException e1) {
            e1.printStackTrace();
        }
        
        
        try {
            adminService.offerCourse("Course 1", "C001", 3, "D1");
            adminService.offerCourse("Course 2", "C002", 3, "D2");
            adminService.offerCourse("Course 3", "C003", 3, "D3");
            adminService.offerCourse("Course 4", "C004", 3, "D4");
        } catch (DuplicateCourseNameException | DuplicateCourseNumberException | InvalidCourseNameException
                | InvalidCourseNumberException | InvalidCourseCreditsException e2) {
            e2.printStackTrace();
        }
        
        
        try {
            adminService.addRoom("B001", "R001", 10);
            adminService.addRoom("B001", "R002", 100);
            adminService.addRoom("B002", "R001", 20);
            adminService.addRoom("B002", "R002", 1);
        } catch (InvalidBuildingNumberException | InvalidRoomNumberException | DuplicateBuildingRoomException e3) {
            e3.printStackTrace();
        }
        
        
        try {
            scheduleIdList.add(adminService.addSchedule("mon", LocalTime.of(10, 0), 60, new BuildingRoom("B001", "R001", 10)));
            scheduleIdList.add(adminService.addSchedule("mon", LocalTime.of(10, 0), 60, new BuildingRoom("B001", "R002", 100)));
            scheduleIdList.add(adminService.addSchedule("mon", LocalTime.of(10, 0), 60, new BuildingRoom("B002", "R001", 20)));
            scheduleIdList.add(adminService.addSchedule("tues", LocalTime.of(13, 30), 120, new BuildingRoom("B002", "R001", 20)));
            scheduleIdList.add(adminService.addSchedule("thursday", LocalTime.of(13, 30), 120, new BuildingRoom("B002", "R001", 20)));
        } catch (DuplicateScheduleException e4) {
            e4.printStackTrace();
        }
        
        
        try {
            adminService.addSeminar("SM01", "C001", 10, instructorIdList.get(0));
            adminService.addSeminar("SM02", "C002", 20, instructorIdList.get(1));
            adminService.addSeminar("SM03", "C003", 30, instructorIdList.get(2));
            adminService.addSeminar("SM04", "C004", 10, instructorIdList.get(0));
        } catch (DuplicateSeminarException | CourseNumberNotFoundException | InstructorIdNotFoundException e5) {
            e5.printStackTrace();
        }
        
        try {
            adminService.addSeminarSchedule("SM01", scheduleIdList.get(0));
            adminService.addSeminarSchedule("SM01", scheduleIdList.get(3));
            adminService.addSeminarSchedule("SM02", scheduleIdList.get(2));
            adminService.addSeminarSchedule("SM03", scheduleIdList.get(1));
        } catch (DuplicateScheduleException | SeminarIdNotFoundException | ScheduleIdNotFoundException
                | SeminarRoomCapacityConflictException | ScheduleNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
    
    @Test
    public void testHireNewInstructor() throws InvalidInstructorNameException, InvalidInstructorEmailException {
        ArrayList<InstructorEmployment> expected = new ArrayList<InstructorEmployment>();
        expected.add(new InstructorEmployment(new Instructor("Instructor 1", instructorIdList.get(0), "e1@mail.com")));
        expected.add(new InstructorEmployment(new Instructor("Instructor 2", instructorIdList.get(1), "e2@mail.com")));
        expected.add(new InstructorEmployment(new Instructor("Instructor 3", instructorIdList.get(2), "e3@mail.com")));
        expected.add(new InstructorEmployment(new Instructor("Instructor 4", instructorIdList.get(3), "e4@mail.com")));
        
        instructorIdList.add(adminService.hireNewInstructor("Instructor 5", "e5@mail.com"));
        expected.add(new InstructorEmployment(new Instructor("Instructor 5", instructorIdList.get(4), "e5@mail.com")));

        assertTrue(compareInstructorEmploymentArrayList(expected, adminService.getAllInstructorEmployments()));
    }

    @Test
    public void testTerminateExistingInstructor() throws InstructorIdNotFoundException {
        ArrayList<InstructorEmployment> expected = new ArrayList<InstructorEmployment>();
        expected.add(new InstructorEmployment(new Instructor("Instructor 1", instructorIdList.get(0), "e1@mail.com")));
        expected.add(new InstructorEmployment(new Instructor("Instructor 2", instructorIdList.get(1), "e2@mail.com")));
        expected.add(new InstructorEmployment(new Instructor("Instructor 3", instructorIdList.get(2), "e3@mail.com")));
        
        adminService.terminateExistingInstructor(instructorIdList.get(3));

        assertTrue(compareInstructorEmploymentArrayList(expected, adminService.getAllEmployedInstructorEmployments()));
        
        expected.add(new InstructorEmployment(new Instructor("Instructor 4", instructorIdList.get(3), "e4@mail.com")));
        
        assertTrue(compareInstructorEmploymentArrayList(expected, adminService.getAllInstructorEmployments()));
    }
    
    @Test
    public void testUpdateInstructorEmploymentName() throws InstructorIdNotFoundException, InvalidInstructorNameException {
        String expected = "Bob";
        
        adminService.updateInstructorEmploymentName(instructorIdList.get(0), "Bob");
        
        assertEquals(expected, adminService.searchForInstructorEmploymentByInstructorId(instructorIdList.get(0)).getInstructor().getName());
    }
    
    @Test
    public void testUpdateInstructorEmploymentEmail() throws InstructorIdNotFoundException, InvalidInstructorEmailException {
        String expected = "Bob";
        
        adminService.updateInstructorEmploymentEmail(instructorIdList.get(0), "Bob");
        
        assertEquals(expected, adminService.searchForInstructorEmploymentByInstructorId(instructorIdList.get(0)).getInstructor().getEmail());
    }
    
    @Test
    public void testUpdateInstructorEmploymentStatus() throws InstructorIdNotFoundException, InvalidInstructorEmailException {
        ArrayList<InstructorEmployment> expected = new ArrayList<InstructorEmployment>();
        expected.add(new InstructorEmployment(new Instructor("Instructor 1", instructorIdList.get(0), "e1@mail.com")));
        expected.add(new InstructorEmployment(new Instructor("Instructor 2", instructorIdList.get(1), "e2@mail.com")));
        expected.add(new InstructorEmployment(new Instructor("Instructor 3", instructorIdList.get(2), "e3@mail.com")));
        
        adminService.updateInstructorEmploymentStatus(instructorIdList.get(3), "terminated");
        
        assertTrue(compareInstructorEmploymentArrayList(expected, adminService.getAllEmployedInstructorEmployments()));
        
        expected.add(new InstructorEmployment(new Instructor("Instructor 4", instructorIdList.get(3), "e4@mail.com")));
        
        assertTrue(compareInstructorEmploymentArrayList(expected, adminService.getAllInstructorEmployments()));
    }
    
    @Test
    public void testUpdateInstructorEmploymentDate() throws InstructorIdNotFoundException {
        LocalDate expected = LocalDate.of(1991, 4, 17);
        
        adminService.updateInstructorEmploymentDate(instructorIdList.get(0), LocalDate.of(1991, 4, 17));
        
        assertEquals(expected, adminService.searchForInstructorEmploymentByInstructorId(instructorIdList.get(0)).getEmploymentDate());
    }
    
    @Test
    public void testSearchForInstructorEmploymentByInstructorId() throws InstructorIdNotFoundException {
        InstructorEmployment expected = new InstructorEmployment(new Instructor("Instructor 1", instructorIdList.get(0), "e1@mail.com"));
        
        assertTrue(expected.instructorIdEquals(adminService.searchForInstructorEmploymentByInstructorId(instructorIdList.get(0))));
    }

    @Test
    public void testSearchForInstructorEmploymentsByInstructorName() {
        ArrayList<InstructorEmployment> expected = new ArrayList<InstructorEmployment>();
        expected.add(new InstructorEmployment(new Instructor("Instructor 2", instructorIdList.get(1), "e2@mail.com")));
        
        assertTrue(compareInstructorEmploymentArrayList(expected, adminService.searchForInstructorEmploymentsByInstructorName("Instructor 2")));
    }

    @Test
    public void testGetAllInstructorEmploymentsByEmploymentDate() throws InstructorIdNotFoundException {
        ArrayList<InstructorEmployment> expected = new ArrayList<InstructorEmployment>();
        
        expected.add(new InstructorEmployment(new Instructor("Instructor 1", instructorIdList.get(0), "e1@mail.com")));
        expected.add(new InstructorEmployment(new Instructor("Instructor 2", instructorIdList.get(1), "e2@mail.com")));
        expected.add(new InstructorEmployment(new Instructor("Instructor 4", instructorIdList.get(3), "e4@mail.com")));
        expected.add(new InstructorEmployment(new Instructor("Instructor 3", instructorIdList.get(2), "e3@mail.com"), LocalDate.of(1991, 4, 17)));
        
        adminService.updateInstructorEmploymentDate(instructorIdList.get(2), LocalDate.of(1991, 4, 17));
        
        assertTrue(compareInstructorEmploymentArrayList(expected, adminService.getAllInstructorEmploymentsByEmploymentDate("employed")));
    }
    
    @Test
    public void testOfferCourse() throws DuplicateCourseNameException, DuplicateCourseNumberException, InvalidCourseNameException, InvalidCourseNumberException, InvalidCourseCreditsException {
        ArrayList<Course> expected = new ArrayList<Course>();
        expected.add(new Course("Course 1", "C001", 3.0, "D1"));
        expected.add(new Course("Course 2", "C002", 2.0, "D2"));
        expected.add(new Course("Course 3", "C003", 1.0, "D3"));
        expected.add(new Course("Course 4", "C004", 5.0, "D4"));
        expected.add(new Course("Course 5", "C005", 3.0, "D5"));
        
        adminService.offerCourse("Course 5", "C005", 3, "D5");
        
        assertTrue(compareCourseArrayList(expected, adminService.getAllOfferedCourses()));
    }
    
    @Test
    public void testCancelCourse() throws CourseNumberNotFoundException {
        ArrayList<Course> expected = new ArrayList<Course>();
        expected.add(new Course("Course 1", "C001", 3.0, "D1"));
        expected.add(new Course("Course 2", "C002", 2.0, "D2"));
        expected.add(new Course("Course 3", "C003", 1.0, "D3"));
        
        adminService.cancelCourse("C004");
        
        assertTrue(compareCourseArrayList(expected, adminService.getAllOfferedCourses()));
    }
    
    @Test
    public void testUpdateCourseName() throws CourseNumberNotFoundException, InvalidCourseNameException, DuplicateCourseNameException {
        String expected = "Bob";
        adminService.updateCourseName("C002", "Bob");
        
        assertEquals(expected, adminService.searchCourseByNumber("C002").getName());
    }
    
    @Test
    public void testUpdateCourseCredit() throws CourseNumberNotFoundException, InvalidCourseCreditsException {
        double expected = 4;
        
        adminService.updateCourseCredit("C002", 4);
        
        assertEquals(expected, adminService.searchCourseByNumber("C002").getCredit(), 0.0000001);
    }
    
    @Test
    public void testUpdateCourseDescription() throws CourseNumberNotFoundException {
        String expected = "Bob";
        adminService.updateCourseDescription("C002", "Bob");
        
        assertEquals(expected, adminService.searchCourseByNumber("C002").getDescription());
    }
    
    @Test
    public void testGetAllOfferedCourses() {
        ArrayList<Course> expected = new ArrayList<Course>();
        expected.add(new Course("Course 1", "C001", 3.0, "D1"));
        expected.add(new Course("Course 2", "C002", 2.0, "D2"));
        expected.add(new Course("Course 3", "C003", 1.0, "D3"));
        expected.add(new Course("Course 4", "C004", 5.0, "D4"));
        
        assertTrue(compareCourseArrayList(expected, adminService.getAllOfferedCourses()));
    }

    @Test
    public void testSearchCourseByName() throws CourseNameNotFoundException {
        Course expected = new Course("Course 3", "C003", 1.0, "D3");
        
        assertEquals(expected.getCourseNumber(), adminService.searchCourseByName("Course 3").getCourseNumber());
    }

    @Test
    public void testSearchCourseByNumber() throws CourseNumberNotFoundException {
        Course expected = new Course("Course 3", "C003", 1.0, "D3");
        
        assertEquals(expected.getCourseNumber(), adminService.searchCourseByNumber("C003").getCourseNumber());
    }
    
    @Test
    public void testAddRoom() throws InvalidBuildingNumberException, InvalidRoomNumberException, DuplicateBuildingRoomException {
        ArrayList<BuildingRoom> expected = new ArrayList<BuildingRoom>();
        expected.add(new BuildingRoom("B001", "R001", 10));
        expected.add(new BuildingRoom("B001", "R002", 100));
        expected.add(new BuildingRoom("B002", "R001", 20));
        expected.add(new BuildingRoom("B002", "R002", 1));
        expected.add(new BuildingRoom("B002", "R003", 12));
        
        adminService.addRoom("B002", "R003", 12);
        
        assertTrue(compareBuildingRoomArrayList(expected, adminService.getAllRooms()));
    }

    @Test
    public void testRemoveRoom() throws BuildingRoomNotFoundException {
        ArrayList<BuildingRoom> expected = new ArrayList<BuildingRoom>();
        expected.add(new BuildingRoom("B001", "R001", 10));
        expected.add(new BuildingRoom("B001", "R002", 100));
        expected.add(new BuildingRoom("B002", "R002", 1));
        
        adminService.removeRoom("B002", "R001");
        
        assertTrue(compareBuildingRoomArrayList(expected, adminService.getAllRooms()));
    }
    
    @Test
    public void testUpdateRoomCapacity() throws BuildingRoomNotFoundException {
        int expected = 55;
        
        adminService.updateRoomCapacity("B002", "R001", 55);
        
        assertEquals(expected, adminService.searchRoomsByBuildingNumber("B002").get(0).getCapacity());
    }

    @Test
    public void testSearchRoomsByBuildingNumber() {
        ArrayList<BuildingRoom> expected = new ArrayList<BuildingRoom>();
        expected.add(new BuildingRoom("B002", "R001", 20));
        expected.add(new BuildingRoom("B002", "R002", 1));
        
        assertTrue(compareBuildingRoomArrayList(expected, adminService.searchRoomsByBuildingNumber("B002")));
    }

    @Test
    public void testSearchRoomsByMinCapacity() {
        ArrayList<BuildingRoom> expected = new ArrayList<BuildingRoom>();
        expected.add(new BuildingRoom("B001", "R002", 100));
        expected.add(new BuildingRoom("B002", "R001", 20));
        
        assertTrue(compareBuildingRoomArrayList(expected, adminService.searchRoomsByMinCapacity(20)));
    }

    @Test
    public void testAddSchedule() throws DuplicateScheduleException {
        ArrayList<Schedule> expected = new ArrayList<Schedule>();
        expected.add(new Schedule(scheduleIdList.get(4), "thursday", LocalTime.of(13, 30), 120, new BuildingRoom("B002", "R001", 20)));
        
        scheduleIdList.add(adminService.addSchedule("wed", LocalTime.of(13, 30), 120, new BuildingRoom("B002", "R001", 20)));

        expected.add(new Schedule(scheduleIdList.get(5), "wed", LocalTime.of(13, 30), 120, new BuildingRoom("B002", "R001", 20)));
        
        assertTrue(compareScheduleArrayList(expected, adminService.getAllAvailableSchedules()));
    }

    @Test
    public void testRemoveSchedule() throws ScheduleIdNotFoundException {
        ArrayList<Schedule> expected = new ArrayList<Schedule>();
        
        adminService.removeSchedule(scheduleIdList.get(4));
        
        assertTrue(compareScheduleArrayList(expected, adminService.getAllAvailableSchedules()));
    }

    @Test
    public void testUpdateScheduleDay() throws ScheduleIdNotFoundException, DuplicateScheduleException {
        String expected = "Friday";
        
        adminService.updateScheduleDay(scheduleIdList.get(4), "fri");
        
        assertEquals(expected, adminService.getAllAvailableSchedules().get(0).getDay());
    }
    
    @Test
    public void testUpdateScheduleStartTime() throws ScheduleIdNotFoundException, DuplicateScheduleException {
        LocalTime expected = LocalTime.of(2,2);
        
        adminService.updateScheduleStartTime(scheduleIdList.get(4), LocalTime.of(2,2));
        
        assertEquals(expected, adminService.getAllAvailableSchedules().get(0).getStartTime());
    }
    
    @Test
    public void testUpdateScheduleDuration() throws ScheduleIdNotFoundException, DuplicateScheduleException {
        int expected = 1;
        
        adminService.updateScheduleDuration(scheduleIdList.get(4), 1);
        
        assertEquals(expected, adminService.getAllAvailableSchedules().get(0).getDuration());
    }
    @Test
    public void testUpdateScheduleLocation() throws ScheduleIdNotFoundException, DuplicateScheduleException {
        BuildingRoom expected = new BuildingRoom("B12", "R12", 100);
        
        adminService.updateScheduleLocation(scheduleIdList.get(4), new BuildingRoom("B12", "R12", 100));
        
        assertTrue(expected.equalBuildNumRoomNum(adminService.getAllAvailableSchedules().get(0).getLocation()));
    }

    @Test
    public void testSearchSchedulesByLocation() {
        BuildingRoom expected = new BuildingRoom("B002", "R001", 20);
        
        assertTrue(expected.equalBuildNumRoomNum(adminService.getAllAvailableSchedules().get(0).getLocation()));
    }

    @Test
    public void testSearchSchedulesByMinCapacity() {
        
    }

    @Test
    public void testSearchSchedulesByTimeBlock() {
        
    }

    @Test
    public void testSearchSchedulesByTimeBlockAndCapacity() {
        
    }
    
    @Test
    public void testAddSeminar() {
        ArrayList<Seminar> expected = new ArrayList<Seminar>();
        expected.add(new Seminar("SM01", new Course("Course One", "C001", 3, "Words"), 20, new Instructor("Name 1", "111111", "a@b.com")));
    }

    @Test
    public void testCancelSeminar() {
        
    }

    @Test
    public void testUpdateSeminarCourse() {
        
    }

    @Test
    public void testUpdateSeminarCapacity() {
        
    }

    @Test
    public void testUpdateSeminarInstructor() {
        
    }

    @Test
    public void testAddSeminarSchedule() {
        
    }
    
    @Test
    public void testRemoveSeminarSchedule() {
        
    }
    
    @Test
    public void testGetSeminarScheduleListBySeminarId() {
        
    }
    
    private boolean compareBuildingRoomArrayList(ArrayList<BuildingRoom> list1, ArrayList<BuildingRoom> list2) { 
        if (list1.size() == list2.size()) {
            Collections.sort(list1);
            Collections.sort(list2);
            for (int i = 0; i < list1.size(); i++) {
                if (!list1.get(i).equalBuildNumRoomNum(list2.get(i))) {
                    return false;
                }
            }
        } else {
            return false;
        }
        return true;
    }
    
    private boolean compareSeminarArrayList(ArrayList<Seminar> list1, ArrayList<Seminar> list2) { 
        if (list1.size() == list2.size()) {
            Collections.sort(list1);
            Collections.sort(list2);
            for (int i = 0; i < list1.size(); i++) {
                if (!list1.get(i).seminarIdEquals(list2.get(i))) {
                    return false;
                }
            }
        } else {
            return false;
        }
        return true;
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
    
    private boolean compareInstructorEmploymentArrayList(ArrayList<InstructorEmployment> list1, ArrayList<InstructorEmployment> list2) { 
        if (list1.size() == list2.size()) {
            Collections.sort(list1);
            Collections.sort(list2);
            for (int i = 0; i < list1.size(); i++) {
                if (!list1.get(i).instructorIdEquals(list2.get(i))) {
                    return false;
                }
            }
        } else {
            return false;
        }
        return true;
    }
    
    private boolean compareCourseArrayList(ArrayList<Course> list1, ArrayList<Course> list2) { 
        if (list1.size() == list2.size()) {
            Collections.sort(list1);
            Collections.sort(list2);
            for (int i = 0; i < list1.size(); i++) {
                if (!list1.get(i).getCourseNumber().equals(list2.get(i).getCourseNumber())) {
                    return false;
                }
            }
        } else {
            return false;
        }
        return true;
    }
}
