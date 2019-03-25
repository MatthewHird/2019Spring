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
import ca.viu.csci331.instruction.model.InstructorEmployment;
import ca.viu.csci331.instruction.model.Schedule;
import ca.viu.csci331.instruction.model.Seminar;

/**
 * Service for a university computer system that tracks and controls instructor
 * employment, offered courses, room availability, and course seminars.
 * 
 * @author Matthew Hird
 * @date Mar. 25, 2019
 */
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
    /**
     * Adds new InstructorEmployment to list with employmentDate = LocalDate.now()
     * and employmentStatus = "employed". A unique instructor ID is randomly 
     * generated between "100000" and "999999".
     * 
     * @param instructorName    Instructor name.
     * @param instructorEmail   Instructor email.
     * @return  The unique instructorId assigned to this instructor.
     * @throws InvalidInstructorNameException   If instructorName is whitespace.
     * @throws InvalidInstructorEmailException  If instructorEmail is whitespace.
     */
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
            employmentList.add(instructorName, instructorId, instructorEmail);
        } catch (DuplicateInstructorEmploymentException e) {
            e.printStackTrace();
        }
        return instructorId;
    }
    
    /**
     * Changes employment status of specified instructor to "terminated".
     * 
     * @param terminatedInstructorId    ID of Instructor to terminate.
     * @throws InstructorIdNotFoundException    If InstructorEmployment with
     *                  instructor ID terminatedInstructorId is not in system.
     */
    public void terminateExistingInstructor(String terminatedInstructorId) throws InstructorIdNotFoundException {
        employmentList.searchByInstructorId(terminatedInstructorId).setEmploymentStatus("terminated");
    }

    /**
     * @param instructorId          Instructor ID.
     * @param newInstructorName     New instructor name
     * @throws InstructorIdNotFoundException    If InstructorEmployment with
     *                  instructor ID terminatedInstructorId is not in system.
     * @throws InvalidInstructorNameException   If newInstructorName is whitespace.
     */
    public void updateInstructorEmploymentName(String instructorId, String newInstructorName) 
            throws InstructorIdNotFoundException, InvalidInstructorNameException {
        if (newInstructorName.trim().equals("")) {
            throw new InvalidInstructorNameException();
        }
        employmentList.searchByInstructorId(instructorId).getInstructor().setName(newInstructorName);
    }
    
    /**
     * @param instructorId          Instructor ID.
     * @param newInstructorEmail    New instructor email.
     * @throws InstructorIdNotFoundException    If InstructorEmployment with
     *                  instructor ID terminatedInstructorId is not in system.
     * @throws InvalidInstructorEmailException  If newInstructorEmail is whitespace.
     */
    public void updateInstructorEmploymentEmail(String instructorId, String newInstructorEmail) 
            throws InstructorIdNotFoundException, InvalidInstructorEmailException {
        if (newInstructorEmail.trim().equals("")) {
            throw new InvalidInstructorEmailException();
        }
        employmentList.searchByInstructorId(instructorId).getInstructor().setEmail(newInstructorEmail);
    }
    
    
    /**
     * @param instructorId          Instructor ID.
     * @param newEmploymentStatus   New employmentStatus. Must either be 
     *                              "employed" or "terminated".
     * @throws InstructorIdNotFoundException    If InstructorEmployment with
     *                  instructor ID terminatedInstructorId is not in system.
     */
    public void updateInstructorEmploymentStatus(String instructorId, String newEmploymentStatus) 
            throws InstructorIdNotFoundException {
        employmentList.searchByInstructorId(instructorId).setEmploymentStatus(newEmploymentStatus);
    }
    
    /**
     * @param instructorId          Instructor ID.
     * @param newEmploymentDate     New employment date.
     * @throws InstructorIdNotFoundException    If InstructorEmployment with
     *                  instructor ID terminatedInstructorId is not in system.
     */
    public void updateInstructorEmploymentDate(String instructorId, LocalDate newEmploymentDate) 
            throws InstructorIdNotFoundException {
        employmentList.searchByInstructorId(instructorId).setEmploymentDate(newEmploymentDate);
    }
    
    
    /**
     * Find InstructorEmployment based on specified instructor ID.
     * 
     * @param instructorId  Instructor ID.
     * @return  InstructorEmployment with instructor ID = instructorId.
     * @throws InstructorIdNotFoundException    If InstructorEmployment with
     *                  instructor ID terminatedInstructorId is not in system.
     */
    public InstructorEmployment searchForInstructorEmploymentByInstructorId(String instructorId) throws InstructorIdNotFoundException {
        InstructorEmployment searchedInstructorEmployment = employmentList.searchByInstructorId(instructorId);
        if (searchedInstructorEmployment.getEmploymentStatus().equals("terminated")) {
            throw new InstructorIdNotFoundException(instructorId);
        }
        return employmentList.searchByInstructorId(instructorId);
    }
    
    
    /**
     * Find all InstructorEmployments with specified instructor name.
     * 
     * @param instructorName    Instructor name.
     * @return  List of InstructorEmployments with instructor 
     *          name == instructorName.
     */
    public ArrayList<InstructorEmployment> searchForInstructorEmploymentsByInstructorName(String instructorName) {
        return AdministrationServiceHelper.employmentTypeFilter(employmentList.searchByInstructorName(instructorName), "employed");
    }
    
    
    /**
     * Get a list of InstructorEmployments with specified employment status type
     * ordered by employmentDate.
     * 
     * @param employmentType    Specified employment status. Must be "employed",
     *                          "terminated", or "both".
     * @return  List of InstructorEmployments with employment 
     *                          status = employmentType.
     */
    public ArrayList<InstructorEmployment> getAllInstructorEmploymentsOrderedByEmploymentDate(String employmentType) {
        ArrayList<InstructorEmployment> instructorEmployments = employmentList.getAllEmployedInstructorEmployments();
        Collections.sort(instructorEmployments, InstructorEmployment.getDateComparator());
        return instructorEmployments;
    }
    
    /**
     * @return  List of all instructors in system.
     */
    public ArrayList<InstructorEmployment> getAllInstructorEmployments() {
        return employmentList.getAllInstructorEmployments();
    }
    
    /**
     * @return  List of all instructor in system with employmentStatus = "employed".
     */
    public ArrayList<InstructorEmployment> getAllEmployedInstructorEmployments() {
        return employmentList.getAllEmployedInstructorEmployments();
    }
//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~END OF INSTRUCTOR SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~START OF COURSE SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    /**
     * @param courseName            Course name.
     * @param courseNumber          Course number.
     * @param courseCredit          Course credit value.
     * @param courseDescription     Course description.
     * @return  true if adding course was successful.
     * @throws DuplicateCourseNameException     If Course with name courseName 
     *                                          is already in system.
     * @throws DuplicateCourseNumberException   If Course with number 
     *                                          courseNumber is already in system.
     * @throws InvalidCourseNameException       If course name is whitespace.
     * @throws InvalidCourseNumberException     If course number is whitespace.
     * @throws InvalidCourseCreditsException    If credit value is < 0.
     */
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
    
    /**
     * @param courseNumber  Course number.
     * @return  Removed Course.
     * @throws CourseNumberNotFoundException    If Course with course number
     *                                          courseNumber is not in system.
     */
    public Course cancelCourse(String courseNumber) throws CourseNumberNotFoundException {
        return courseList.remove(courseNumber);
    }
    
    /**
     * @param courseNumber  Course number.
     * @param courseName    New course name.
     * @throws CourseNumberNotFoundException    If Course with course number
     *                                          courseNumber is not in system.
     * @throws InvalidCourseNameException       If courseName is whitespace.
     * @throws DuplicateCourseNameException     If course with Course name
     *                                          courseName is already in system.
     */
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
    
    /**
     * @param courseNumber  Course number.
     * @param courseCredit  New credit value.
     * @throws CourseNumberNotFoundException    If Course with course number
     *                                          courseNumber is not in system.
     * @throws InvalidCourseCreditsException    If courseCredit < 0.
     */
    public void updateCourseCredit(String courseNumber, double courseCredit) 
            throws CourseNumberNotFoundException, InvalidCourseCreditsException {
        if (courseCredit < 0) {
            throw new InvalidCourseCreditsException();
        }
        courseList.searchByNumber(courseNumber).setCredit(courseCredit);
    }
    
    /**
     * @param courseNumber          Couse number.
     * @param courseDescription     New course description.
     * @throws CourseNumberNotFoundException    If Course with couse number
     *                                          courseNumber is not in system.
     */
    public void updateCourseDescription(String courseNumber, String courseDescription) 
            throws CourseNumberNotFoundException {
        courseList.searchByNumber(courseNumber).setDescription(courseDescription);
    }
    
    /**
     * @return  List of all Courses in system.
     */
    public ArrayList<Course> getAllOfferedCourses() {
        return courseList.getAllOfferedCourses();
    }
    
    /**
     * Find Course based on specified couse name.
     * 
     * @param courseName    Course name.
     * @return  Course with course name == courseName.
     * @throws CourseNameNotFoundException  If Course with name courseName is 
     *                                      not in system.
     */
    public Course searchCourseByName(String courseName) throws CourseNameNotFoundException {
        return courseList.searchByName(courseName);
    }
    
    /**
     * Find Course with specified course number.
     * 
     * @param courseNumber  Course number.
     * @return  Course with course number == courseNumber.
     * @throws CourseNumberNotFoundException    If Course with course number of
     *                                          courseNumber is not in system.
     */
    public Course searchCourseByNumber(String courseNumber) throws CourseNumberNotFoundException {
        return courseList.searchByNumber(courseNumber);
    }
//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~END OF COURSE SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~START OF BUILDING/ROOM SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    /**
     * @param buildingNumber    Building ID.
     * @param roomNumber        Room Id.
     * @param roomCapacity      Seat capacity.
     * @return  true if adding room was successful.
     * @throws InvalidBuildingNumberException   If buildingNumber is whitespace.
     * @throws InvalidRoomNumberException       If roomNumber is whitespace.
     * @throws DuplicateBuildingRoomException   If rooCapacity < 0.
     */
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
    
    /**
     * @param buildingNumber    Building ID.
     * @param roomNumber        Room ID.
     * @return  Removed BuildingRoom.
     * @throws BuildingRoomNotFoundException    If BuildingRoom with building ID
     *                  buildingNumber and room ID rooNumber is not in system.
     */
    public BuildingRoom removeRoom(String buildingNumber, String roomNumber) 
            throws BuildingRoomNotFoundException {
        return roomList.remove(buildingNumber, roomNumber);
    }
    
    /**
     * @param buildingNumber    Building ID.
     * @param roomNumber        Room ID.
     * @param roomCapacity      New seat capacity.
     * @throws BuildingRoomNotFoundException    If BuildingRoom with building ID
     *                  buildingNumber and room ID rooNumber is not in system.
     */
    public void updateRoomCapacity(String buildingNumber, String roomNumber, int roomCapacity) 
            throws BuildingRoomNotFoundException {
        roomList.searchByBuildNumRoomNum(buildingNumber, roomNumber).setCapacity(roomCapacity);
    }
    
    /**
     * Find all BuildingRooms within specified building.
     * 
     * @param buildingNumber    building ID.
     * @return  List of BuildingRooms with building ID == buildingNumber.
     */
    public ArrayList<BuildingRoom> searchRoomsByBuildingNumber(String buildingNumber) {
        return roomList.searchByBuilding(buildingNumber);
    }
    
    /**
     * Find all BuildingRooms with seat capacity greater than or equal to 
     * specified value.
     * 
     * @param minCapacity   Minimum seat capacity.
     * @return  List of BuildingRooms with capacity >. minCapacity.
     */
    public ArrayList<BuildingRoom> searchRoomsByMinCapacity(int minCapacity) {
        return roomList.searchByMinCapacity(minCapacity);
    }
    
    /**
     * @return  List of all BuildingRooms in system.
     */
    public ArrayList<BuildingRoom> getAllRooms() {
        return roomList.getAllBuildingRooms();
    }
//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~END OF BUILDING/ROOM SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~START OF SCHEDULES SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    /**
     * Add a schedule to the system. A unique scheduleID is randomly generated.
     * 
     * @param dayOfWeek     The day of the week for the schedule block.
     *      Must be one of the folowing values: "sun", "sunday", "mon", 
     * "monday", "tue", "tues", "tuesday", "wed", "wednesday", "thu", "thur", 
     * "thurs", "thursday", "fri", "friday", "sat", or "saturday".
     * 
     * @param startTime     Start time of schedule block.
     * @param duration      Duration of schedule block in minutes.
     * @param location      Room the schedule is for.
     * @return  Unique schedule ID.
     * @throws DuplicateScheduleException
     */
    public String addSchedule(String dayOfWeek, LocalTime startTime, int duration,
            BuildingRoom location) throws DuplicateScheduleException {
        
        String scheduleId = "";
        
        do {
            scheduleId = AdministrationServiceHelper.generateScheduleId();
        } while (availableScheduleList.containsScheduleId(scheduleId));
        
        availableScheduleList.add(new Schedule(scheduleId, dayOfWeek, startTime, duration, location));
        return scheduleId;
    }
    
    /**
     * @param scheduleId    Schedule ID.
     * @return  Removed Schedule.
     * @throws ScheduleIdNotFoundException  If Schedule with ID scheduleId is
     *                                      not in system.
     */
    public Schedule removeSchedule(String scheduleId) throws ScheduleIdNotFoundException {
        return availableScheduleList.remove(scheduleId);
    }
    
    /**
     * @param scheduleId    Schedule ID.
     * @param dayOfWeek     New day of the week for the schedule block.
     *      Must be one of the folowing values: "sun", "sunday", "mon", 
     * "monday", "tue", "tues", "tuesday", "wed", "wednesday", "thu", "thur", 
     * "thurs", "thursday", "fri", "friday", "sat", or "saturday".
     * 
     * @throws ScheduleIdNotFoundException  If Schedule with ID scheduleId is
     *                                      not in system.
     * @throws DuplicateScheduleException   If the new dayOfWeek causes a time
     *                                      overlap with an existing Schedule.
     */
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
    
    /**
     * @param scheduleId    Schedule ID.
     * @param startTime     New start of schedule block.
     * @throws ScheduleIdNotFoundException  If Schedule with ID scheduleId is
     *                                      not in system.
     * @throws DuplicateScheduleException   If the new startTime causes a time
     *                                      overlap with an existing Schedule.
     */
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
    
    /**
     * @param scheduleId    Schedule ID.
     * @param duration      New duration.
     * @throws ScheduleIdNotFoundException  If Schedule with ID scheduleId is
     *                                      not in system.
     * @throws DuplicateScheduleException   If the new duration causes a time
     *                                      overlap with an existing Schedule.
     */
    public void updateScheduleDuration(String scheduleId, int duration) 
            throws ScheduleIdNotFoundException, DuplicateScheduleException {
        Schedule scheduleToUpdate = availableScheduleList.remove(scheduleId);

        int temp = scheduleToUpdate.getDuration();
        
        scheduleToUpdate.setDuration(duration);
        
        if (availableScheduleList.containsOverlap(new Schedule("", scheduleToUpdate.getDay(),
                scheduleToUpdate.getStartTime(), duration, scheduleToUpdate.getLocation()))) {
            scheduleToUpdate.setDuration(temp);
            availableScheduleList.add(scheduleToUpdate);
            scheduleToUpdate.setDuration(temp);
            throw new DuplicateScheduleException();
        }
        availableScheduleList.add(scheduleToUpdate);
    }
    
    /**
     * @param scheduleId    Schedule ID.
     * @param location      New location.
     * @throws ScheduleIdNotFoundException  If Schedule with ID scheduleId is
     *                                      not in system.
     * @throws DuplicateScheduleException   If the new location causes a time
     *                                      overlap with an existing Schedule.
     */
    public void updateScheduleLocation(String scheduleId, BuildingRoom location) 
            throws ScheduleIdNotFoundException, DuplicateScheduleException {
        Schedule scheduleToUpdate = availableScheduleList.searchByScheduleId(scheduleId);
        if (availableScheduleList.containsOverlap(new Schedule("", scheduleToUpdate.getDay(),
                scheduleToUpdate.getStartTime(), scheduleToUpdate.getDuration(), location))) {
            throw new DuplicateScheduleException();
        }
        scheduleToUpdate.setLocation(location);
    }
    
    /**
     * Find all Schedules with specified location.
     * 
     * @param buildingNumber    Building ID.
     * @param roomNumber        Room ID.
     * @return  All schedule with Location building ID == buildingNumber and
     *          room ID == roomNumber.
     */
    public ArrayList<Schedule> searchSchedulesByLocation(String buildingNumber, String roomNumber) {
        return availableScheduleList.searchByLocation(buildingNumber, roomNumber);
    }
    
    /**
     * Find all Schedules with seat capacity greater than or equal to the 
     * specified value.
     * 
     * @param minCapacity   Minimum seat capacity.
     * @return  List schedules with capacity >= minCapacity.
     */
    public ArrayList<Schedule> searchSchedulesByMinCapacity(int minCapacity) {
        return availableScheduleList.searchByMinRoomCapacity(minCapacity);
    }
    
    /**
     * Find all Schedules within specified time block.
     * 
     * @param dayOfWeek         The day of the week for the schedule block.
     *      Must be one of the folowing values: "sun", "sunday", "mon", 
     * "monday", "tue", "tues", "tuesday", "wed", "wednesday", "thu", "thur", 
     * "thurs", "thursday", "fri", "friday", "sat", or "saturday".
     *
     * @param startTime         Start of time block.
     * @param durationMinutes   Duration of time block.
     * @return  All Schedules within specified time block.
     */
    public ArrayList<Schedule> searchSchedulesByTimeBlock(
            String dayOfWeek, LocalTime startTime, int durationMinutes) {
        return availableScheduleList.searchByTimeBlock(dayOfWeek, startTime, durationMinutes);
    }
    
    /**
     * Find all Schedules within specified time block with a seat capacity
     * greater than or equal to the specified minimum capacity.
     * 
     * @param dayOfWeek         The day of the week for the schedule block.
     *      Must be one of the folowing values: "sun", "sunday", "mon", 
     * "monday", "tue", "tues", "tuesday", "wed", "wednesday", "thu", "thur", 
     * "thurs", "thursday", "fri", "friday", "sat", or "saturday".
     *
     * @param startTime         Start of time block.
     * @param durationMinutes   Duration of time block.
     * @param minRoomCapacity   Minimum seat capacity.
     * @return  All Schedules within the specified time block with a seat
     *          capacity >= minCapacity.
     */
    public ArrayList<Schedule> searchSchedulesByTimeBlockAndCapacity(String dayOfWeek, 
            LocalTime startTime, int durationMinutes, int minRoomCapacity) {
        return availableScheduleList.searchByTimeBlockAndCapacity(
                dayOfWeek, startTime, durationMinutes, minRoomCapacity);
    }
    
    /**
     * List of all schedules that are available.
     * 
     * @return  Gets all schedules not currently being used by a seminar.
     */
    public ArrayList<Schedule> getAllAvailableSchedules() {
        return availableScheduleList.getAllSchedules();
    }
//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~END OF SCHEDULES SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~START OF SEMINARS SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    /**
     * @param seminarId     Seminar ID.
     * @param courseNumber  Course number of Course in system.
     * @param capacity      Offered seat capacity.
     * @param instructorId  Instructor ID of InstructorEmployment currently in
     *                      system.
     * @return  true if Seminar is successfully added to system.
     * @throws DuplicateSeminarException        If seminar with ID seminarId is
     *                                          already in system.
     * @throws CourseNumberNotFoundException    If Course with course number
     *                                          courseNumber is not in system.
     * @throws InstructorIdNotFoundException    If InstructorEmployment with
     *                                          instructor id InstructorId is
     *                                          not in system.
     */
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
    
    /**
     * Removes specified Seminar from system.
     * 
     * @param seminarId     Seminar ID.
     * @return  Cancelled seminar.
     * @throws SeminarIdNotFoundException   If Seminar with seminar ID seminarId
     *                                      is not in system.
     */
    public Seminar cancelSeminar(String seminarId) throws SeminarIdNotFoundException {
        return seminarList.remove(seminarId);
    }
    
    /**
     * Updates Course in Seminar to a different Course in the system, specified
     * by courseNumber.
     * 
     * @param seminarId     Seminar ID.
     * @param courseNumber  Course number of new Course.
     * @throws SeminarIdNotFoundException       If Seminar with seminar ID
     *                                          seminarId is not in system.
     * @throws CourseNumberNotFoundException    If Course with course number
     *                                          courseNumber is not in system.
     */
    public void updateSeminarCourse(String seminarId, String courseNumber) 
            throws SeminarIdNotFoundException, CourseNumberNotFoundException {
        seminarList.searchBySeminarId(seminarId).setCourse(courseList.searchByNumber(courseNumber));
    }
    
    /**
     * @param seminarId         Seminar ID.
     * @param updatedCapacity   New offered capacity.
     * @throws SeminarIdNotFoundException           If Seminar with seminar ID
     *                                              seminarId is not in system.
     * @throws SeminarRoomCapacityConflictException     If change in capacity 
     *                      results in a conflict with the seat capacity of a 
     *                      schedule assigned to specified Seminar.
     */
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
    
    /**
     * Updates Instructor in Seminar to an Instructor in the system with 
     * specified instructor ID.
     * 
     * @param seminarId     Seminar ID.
     * @param instructorId  Instructor ID of new Instructor.
     * @throws InstructorIdNotFoundException    If InstructorEmplymoent with 
     *                                          instructor ID instructorId does
     *                                          not exist in system.
     * @throws SeminarIdNotFoundException       If Seminar with seminar ID  
     *                                          seminarId is not in system.
     */
    public void updateSeminarInstructor(String seminarId, String instructorId) 
            throws InstructorIdNotFoundException, SeminarIdNotFoundException {
        seminarList.searchBySeminarId(seminarId).setInstructor(employmentList
                .searchByInstructorId(instructorId).getInstructor());
    }
    
    /**
     * @param seminarId     Seminar ID.
     * @param scheduleId    Schedule ID of new schedule.
     * @throws DuplicateScheduleException           If Schedule with sechedule ID
     *                      scheduleId is already in seminarScheduleList.
     * @throws SeminarIdNotFoundException           If Seminar with seminar ID
     *                      seminarId is not in the system.
     * @throws ScheduleIdNotFoundException          If Schedule with schedule ID
     *                      scheduleId is not currently available in system.
     * @throws SeminarRoomCapacityConflictException If the offered capacity of
     *                      the Seminar > seat capacity of Schedule.
     * @throws ScheduleNotFoundException            If Schedule with schedule ID
     *                      scheduleId is not currently available in system.
     */
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
    
    /**
     * @param seminarId     Seminar ID.
     * @param scheduleId    Schedule ID.
     * @throws ScheduleIdNotFoundException  If Schedule with schedule ID 
     *                                      scheduleID is not in seminarScheduleList.
     * @throws SeminarIdNotFoundException   If Seminar with seminar ID seminarId
     *                                      is not in system.
     */
    public void removeSeminarSchedule(String seminarId, String scheduleId) 
            throws ScheduleIdNotFoundException, 
            SeminarIdNotFoundException {
        Seminar seminar = seminarList.searchBySeminarId(seminarId);
        Schedule schedule = usedScheduleList.searchByScheduleId(scheduleId);
        
        seminar.removeSchedule(scheduleId);
        try {
            availableScheduleList.add(schedule);
        } catch (DuplicateScheduleException e) {
            e.printStackTrace();
        }
        usedScheduleList.remove(scheduleId);
    }
    
    /**
     * Find a Seminar with speficied seminar ID.
     * 
     * @param seminarId Seminar ID.
     * @return  Seminar with seminar ID = seminarId.
     * @throws SeminarIdNotFoundException   If Seminar with seminar ID seminarId
     *                                      does not exist in system.
     */
    public Seminar searchSeminarById(String seminarId) throws SeminarIdNotFoundException {
        return seminarList.searchBySeminarId(seminarId);
    }
    
    /**
     * Get all Schedules assigned to speficied Seminar.
     * 
     * @param seminarId     Seminar ID.
     * @return  List of Schedules assigned to Seminar with seminar ID = seminarId.
     * @throws SeminarIdNotFoundException   If Seminar with seminar ID seminarId
     *                                      does not exists in system.
     */
    public ArrayList<Schedule> getSeminarScheduleListBySeminarId(String seminarId) throws SeminarIdNotFoundException {
        return seminarList.searchBySeminarId(seminarId).getSeminarScheduleList();
    }
    
    /**
     * @return  List of all Seminars in system.
     */
    public ArrayList<Seminar> getAllSeminars() {
        return seminarList.getAllSeminars();
    }
//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~END OF SEMINARS SECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
