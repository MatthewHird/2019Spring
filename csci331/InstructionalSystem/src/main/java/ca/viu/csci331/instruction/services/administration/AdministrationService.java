package ca.viu.csci331.instruction.services.administration;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Collections;

import ca.viu.csci331.instruction.exception.BuildingRoomNotFoundException;
import ca.viu.csci331.instruction.exception.CourseNameNotFoundException;
import ca.viu.csci331.instruction.exception.CourseNumberNotFoundException;
import ca.viu.csci331.instruction.exception.DuplicateBuildingRoomException;
import ca.viu.csci331.instruction.exception.DuplicateCourseNameException;
import ca.viu.csci331.instruction.exception.DuplicateCourseNumberException;
import ca.viu.csci331.instruction.exception.DuplicateInstructorEmploymentException;
import ca.viu.csci331.instruction.exception.DuplicateScheduleException;
import ca.viu.csci331.instruction.exception.DuplicateSeminarException;
import ca.viu.csci331.instruction.exception.InvalidInstructorEmailException;
import ca.viu.csci331.instruction.exception.InvalidInstructorNameException;
import ca.viu.csci331.instruction.exception.InvalidRoomNumberException;
import ca.viu.csci331.instruction.exception.ScheduleIdNotFoundException;
import ca.viu.csci331.instruction.exception.ScheduleNotFoundException;
import ca.viu.csci331.instruction.exception.SeminarIdNotFoundException;
import ca.viu.csci331.instruction.exception.SeminarRoomCapacityConflictException;
import ca.viu.csci331.instruction.exception.InstructorIdNotFoundException;
import ca.viu.csci331.instruction.exception.InvalidBuildingNumberException;
import ca.viu.csci331.instruction.exception.InvalidCourseCreditsException;
import ca.viu.csci331.instruction.exception.InvalidCourseNameException;
import ca.viu.csci331.instruction.exception.InvalidCourseNumberException;
import ca.viu.csci331.instruction.model.BuildingRoom;
import ca.viu.csci331.instruction.model.Course;
import ca.viu.csci331.instruction.model.Instructor;
import ca.viu.csci331.instruction.model.InstructorEmployment;
import ca.viu.csci331.instruction.model.Schedule;
import ca.viu.csci331.instruction.model.Seminar;

public class AdministrationService {
    private InstructorEmploymentList employmentList;
    private OfferedCourseList courseList;
    private BuildingRoomList roomList;
    private ScheduleList availableScheduleList;
    private ScheduleList usedScheduleList;
    private SeminarList seminarList;
    
    
    
    public AdministrationService() {
        employmentList = new InstructorEmploymentList();
        courseList = new OfferedCourseList();
        roomList = new BuildingRoomList();
        availableScheduleList = new ScheduleList();
        usedScheduleList = new ScheduleList();
        seminarList = new SeminarList();
    }
    
    
//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~START OF INSTRUCTOR SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    public String hireNewInstructor(String instructorName, String instructorEmail) throws InvalidInstructorNameException, 
            InvalidInstructorEmailException {

        String instructorId;

        if (instructorName.trim().equals("")) {
            throw new InvalidInstructorNameException();
        }        

        if (instructorEmail.trim().equals("")) {
            throw new InvalidInstructorEmailException();
        }
        
        do {
            instructorId = AdministrationServiceHelper.generateInstructorId();
        } while (employmentList.containsInstructorId(instructorId));
        
        try {
            employmentList.add(new InstructorEmployment(new Instructor(instructorName, instructorId, instructorEmail)));
        } catch (DuplicateInstructorEmploymentException e) {
            e.printStackTrace();
        }
        return instructorId;
    }
    
    
    public void terminateExistingInstructor(String terminatedInstructorId) throws InstructorIdNotFoundException {
        employmentList.searchByInstructorId(terminatedInstructorId).setEmploymentStatus("terminated");
    }


    public void updateInstructorEmploymentName(String instructorId, String newInstructorName) 
            throws InstructorIdNotFoundException, InvalidInstructorNameException {
        if (newInstructorName.trim().equals("")) {
            throw new InvalidInstructorNameException();
        }
        employmentList.searchByInstructorId(instructorId).getInstructor().setName(newInstructorName);
    }
    
    
    public void updateInstructorEmploymentEmail(String instructorId, String newInstructorEmail) 
            throws InstructorIdNotFoundException, InvalidInstructorEmailException {
        if (newInstructorEmail.trim().equals("")) {
            throw new InvalidInstructorEmailException();
        }
        employmentList.searchByInstructorId(instructorId).getInstructor().setEmail(newInstructorEmail);
    }
    
    
    public void updateInstructorEmploymentStatus(String instructorId, String newEmploymentStatus) 
            throws InstructorIdNotFoundException {
        employmentList.searchByInstructorId(instructorId).setEmploymentStatus(newEmploymentStatus);
    }
    
    public void updateInstructorEmploymentDate(String instructorId, LocalDate newEmploymentDate) 
            throws InstructorIdNotFoundException {
        employmentList.searchByInstructorId(instructorId).setEmploymentDate(newEmploymentDate);
    }
    
    
    public InstructorEmployment searchForInstructorEmploymentByInstructorId(String instructorId) throws InstructorIdNotFoundException {
        InstructorEmployment searchedInstructorEmployment = employmentList.searchByInstructorId(instructorId);
        if (searchedInstructorEmployment.getEmploymentStatus().equals("terminated")) {
            throw new InstructorIdNotFoundException(instructorId);
        }
        return employmentList.searchByInstructorId(instructorId);
    }
    
    
    public ArrayList<InstructorEmployment> searchForInstructorEmploymentsByInstructorName(String instructorName) {
        return AdministrationServiceHelper.employmentTypeFilter(employmentList.searchByInstructorName(instructorName), "employed");
    }
    
    
    public ArrayList<InstructorEmployment> getAllInstructorEmploymentsByEmploymentDate(String employmentType) {
        ArrayList<InstructorEmployment> instructorEmployments = employmentList.getAllEmployedInstructorEmployments();
        Collections.sort(instructorEmployments, InstructorEmployment.getDateComparator());
        return instructorEmployments;
    }
//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~END OF INSTRUCTOR SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~START OF COURSE SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    public boolean offerCourse(String courseName, String courseNumber, double courseCredit, String courseDescription) 
            throws DuplicateCourseNameException, DuplicateCourseNumberException, 
            InvalidCourseNameException, InvalidCourseNumberException, InvalidCourseCreditsException {
        if (courseName.trim().equals("")) {
            throw new InvalidCourseNameException();
        }
        if (courseNumber.trim().equals("")) {
            throw new InvalidCourseNumberException();
        }
        if (courseCredit < 0) {
            throw new InvalidCourseCreditsException();
        }
        courseList.add(courseName, courseNumber, courseCredit, courseDescription);
        return true;
    }
    
    public Course cancelCourse(String courseNumber) throws CourseNumberNotFoundException {
        return courseList.remove(courseNumber);
    }
    
    public void updateCourseName(String courseNumber, String courseName) 
            throws CourseNumberNotFoundException, InvalidCourseNameException, DuplicateCourseNameException {
        if (courseName.trim().equals("")) {
            throw new InvalidCourseNameException();
        }
        if (courseList.containsCourseName(courseName)) {
            throw new DuplicateCourseNameException(courseName);
        }
        courseList.searchByNumber(courseNumber).setName(courseName);
    }
    
    public void updateCourseCredit(String courseNumber, double courseCredit) 
            throws CourseNumberNotFoundException, InvalidCourseCreditsException {
        if (courseCredit < 0) {
            throw new InvalidCourseCreditsException();
        }
        courseList.searchByNumber(courseNumber).setCredit(courseCredit);
    }
    
    public void updateCourseDescription(String courseNumber, String courseDescription) 
            throws CourseNumberNotFoundException {
        courseList.searchByNumber(courseNumber).setDescription(courseDescription);
    }
    
    public ArrayList<Course> getAllOfferedCourses() {
        return courseList.getAllOfferedCourses();
    }
    
    public Course searchCourseByName(String courseName) throws CourseNameNotFoundException {
        return courseList.searchByName(courseName);
    }
    
    public Course searchCourseByNumber(String courseNumber) throws CourseNumberNotFoundException {
        return courseList.searchByNumber(courseNumber);
    }
//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~END OF COURSE SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~START OF BUILDING/ROOM SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    public boolean addRoom(String buildingNumber, String roomNumber, int roomCapacity) 
            throws InvalidBuildingNumberException, InvalidRoomNumberException, 
            DuplicateBuildingRoomException {
        if (buildingNumber.trim().equals("")) {
            throw new InvalidBuildingNumberException();
        }
        if (roomNumber.trim().equals("")) {
            throw new InvalidRoomNumberException();
        }
        roomList.add(buildingNumber, roomNumber, roomCapacity);
        return true;
    }
    
    public BuildingRoom removeRoom(String buildingNumber, String roomNumber) 
            throws BuildingRoomNotFoundException {
        return roomList.remove(buildingNumber, roomNumber);
    }
    
    public void updateRoomCapacity(String buildingNumber, String roomNumber, int roomCapacity) 
            throws BuildingRoomNotFoundException {
        roomList.searchByBuildNumRoomNum(buildingNumber, roomNumber).setCapacity(roomCapacity);
    }
    
    public ArrayList<BuildingRoom> searchRoomsByBuildingNumber(String buildingNumber) {
        return roomList.searchByBuilding(buildingNumber);
    }
    
    public ArrayList<BuildingRoom> searchRoomsByMinCapacity(int minCapacity) {
        return roomList.searchByMinCapacity(minCapacity);
    }
//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~END OF BUILDING/ROOM SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~START OF SCHEDULES SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    public String addSchedule(String dayOfWeek, LocalTime startTime, int duration,
            BuildingRoom location) throws DuplicateScheduleException {
        
        String scheduleId = "";
        
        do {
            scheduleId = AdministrationServiceHelper.generateScheduleId();
        } while (availableScheduleList.containsScheduleId(scheduleId));
        
        availableScheduleList.add(new Schedule(scheduleId, dayOfWeek, startTime, duration, location));
        return scheduleId;
    }
    
    public Schedule removeSchedule(String scheduleId) throws ScheduleIdNotFoundException {
        return availableScheduleList.remove(scheduleId);
    }
    
    public void updateScheduleDay(String scheduleId, String dayOfWeek) 
            throws ScheduleIdNotFoundException, DuplicateScheduleException {
        Schedule scheduleToUpdate = availableScheduleList.searchByScheduleId(scheduleId);
        if (availableScheduleList.containsOverlap(new Schedule("", dayOfWeek,
                scheduleToUpdate.getStartTime(), scheduleToUpdate.getDuration(),
                scheduleToUpdate.getLocation()))) {
            throw new DuplicateScheduleException();
        }
        scheduleToUpdate.setDay(dayOfWeek);
    }
    
    public void updateScheduleStartTime(String scheduleId, LocalTime startTime) 
            throws ScheduleIdNotFoundException, DuplicateScheduleException {
        Schedule scheduleToUpdate = availableScheduleList.searchByScheduleId(scheduleId);
        if (availableScheduleList.containsOverlap(new Schedule("", scheduleToUpdate.getDay(),
                startTime, scheduleToUpdate.getDuration(),
                scheduleToUpdate.getLocation()))) {
            throw new DuplicateScheduleException();
        }
        scheduleToUpdate.setStartTime(startTime);
    }
    
    public void updateScheduleDuration(String scheduleId, int duration) 
            throws ScheduleIdNotFoundException, DuplicateScheduleException {
        Schedule scheduleToUpdate = availableScheduleList.searchByScheduleId(scheduleId);
        if (availableScheduleList.containsOverlap(new Schedule("", scheduleToUpdate.getDay(),
                scheduleToUpdate.getStartTime(), duration,
                scheduleToUpdate.getLocation()))) {
            throw new DuplicateScheduleException();
        }
        scheduleToUpdate.setDuration(duration);
    }
    
    public void updateScheduleLocation(String scheduleId, BuildingRoom location) 
            throws ScheduleIdNotFoundException, DuplicateScheduleException {
        Schedule scheduleToUpdate = availableScheduleList.searchByScheduleId(scheduleId);
        if (availableScheduleList.containsOverlap(new Schedule("", scheduleToUpdate.getDay(),
                scheduleToUpdate.getStartTime(), scheduleToUpdate.getDuration(), location))) {
            throw new DuplicateScheduleException();
        }
        scheduleToUpdate.setLocation(location);
    }
    
    public ArrayList<Schedule> searchSchedulesByLocation(String buildingNumber, String roomNumber) {
        return availableScheduleList.searchByLocation(buildingNumber, roomNumber);
    }
    
    public ArrayList<Schedule> searchSchedulesByLocation(BuildingRoom location) {
        return availableScheduleList.searchByLocation(location);
    }
    
    public ArrayList<Schedule> searchSchedulesByMinCapacity(int minCapacity) {
        return availableScheduleList.searchByMinRoomCapacity(minCapacity);
    }
    
    public ArrayList<Schedule> searchSchedulesByTimeBlock(
            String dayOfWeek, LocalTime startTime, int durationMinutes) {
        return availableScheduleList.searchByTimeBlock(dayOfWeek, startTime, durationMinutes);
    }
    
    public ArrayList<Schedule> searchSchedulesByTimeBlockAndCapacity(String dayOfWeek, 
            LocalTime startTime, int durationMinutes, int minRoomCapacity) {
        return availableScheduleList.searchByTimeBlockAndCapacity(
                dayOfWeek, startTime, durationMinutes, minRoomCapacity);
    }

//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~END OF SCHEDULES SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~START OF SEMINARS SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    public boolean addSeminar(String seminarId, String courseNumber, 
            int capacity, String instructorId) throws DuplicateSeminarException,
            CourseNumberNotFoundException, InstructorIdNotFoundException {
        if (seminarList.containsSeminarId(seminarId)) {
            throw new DuplicateSeminarException(seminarId);
        }
        seminarList.add(seminarId, courseList.searchByNumber(courseNumber), capacity, 
                employmentList.searchByInstructorId(instructorId).getInstructor());
        return true;
    }
    
    public Seminar cancelSeminar(String seminarId) throws SeminarIdNotFoundException {
        return seminarList.remove(seminarId);
    }
    
    public void updateSeminarCourse(String seminarId, String courseNumber) 
            throws SeminarIdNotFoundException, CourseNumberNotFoundException {
        seminarList.searchBySeminarId(seminarId).setCourse(courseList.searchByNumber(courseNumber));
    }
    
    public void updateSeminarCapacity(String seminarId, int updatedCapacity) 
            throws SeminarIdNotFoundException, SeminarRoomCapacityConflictException {
        Seminar seminar = seminarList.searchBySeminarId(seminarId);
        for (Schedule schedule : seminar.getSeminarScheduleList()) {
            if (updatedCapacity > schedule.getLocation().getCapacity()) {
                throw new SeminarRoomCapacityConflictException();
            }
        }
        seminar.setCapacity(updatedCapacity);
    }
    
    public void updateSeminarInstructor(String seminarId, String instructorId) 
            throws InstructorIdNotFoundException, SeminarIdNotFoundException {
        seminarList.searchBySeminarId(seminarId).setInstructor(employmentList
                .searchByInstructorId(instructorId).getInstructor());
    }
    
    public void addSeminarSchedule(String seminarId, String scheduleId) 
            throws DuplicateScheduleException, SeminarIdNotFoundException, 
            ScheduleIdNotFoundException, SeminarRoomCapacityConflictException, 
            ScheduleNotFoundException {
        Seminar seminar = seminarList.searchBySeminarId(seminarId);
        Schedule schedule = availableScheduleList.searchByScheduleId(scheduleId);
        if (seminar.getCapacity() > availableScheduleList.searchByScheduleId(scheduleId).getLocation().getCapacity()) {
            throw new SeminarRoomCapacityConflictException();
        }
        
        seminar.addSchedule(schedule);
        usedScheduleList.add(schedule);
        availableScheduleList.remove(schedule);
    }
    
    public void removeSeminarSchedule(String seminarId, String scheduleId) 
            throws DuplicateScheduleException, ScheduleIdNotFoundException, 
            SeminarIdNotFoundException {
        Seminar seminar = seminarList.searchBySeminarId(seminarId);
        Schedule schedule = usedScheduleList.searchByScheduleId(scheduleId);
        
        seminar.removeSchedule(scheduleId);
        availableScheduleList.add(schedule);
        usedScheduleList.remove(scheduleId);
    }
    
    public ArrayList<Schedule> getSeminarScheduleListBySeminarId(String seminarId) throws SeminarIdNotFoundException {
        return seminarList.searchBySeminarId(seminarId).getSeminarScheduleList();
    }
//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~END OF SEMINARS SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}





































