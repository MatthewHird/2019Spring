package ca.viu.csci331.instruction.model;

import java.util.ArrayList;
import java.util.List;

import ca.viu.csci331.instruction.exception.DuplicateScheduleException;
import ca.viu.csci331.instruction.exception.InvalidSeminarCapacityRuntimeException;
import ca.viu.csci331.instruction.exception.ScheduleIdNotFoundException;
import ca.viu.csci331.instruction.exception.ScheduleNotFoundException;

/**
 *  Represents an instance of a course being offered at a university. Manages the class size capacity,
 *  the course, the instructor, and the lecturelocation and time blocks (<Schedule> objects).
 * @author Matthew Hird
 * @date Feb. 12, 2019
 */
public class Seminar implements Comparable<Object> {
    private String semniarId;
    private Course course;
    private int capacity;
    private Instructor instructor;
    private List<Schedule> seminarScheduleList;
    
    /**
     * Primary constructor
     * @param semniarId  ID for the seminar.
     * @param course     The course the seminar is an instance of.
     * @param capacity   The maximum number of students that can enroll in the seminar.
     * @param instructor The instructor running the seminar.
     */
    public Seminar(String seminarId, Course course, int capacity, Instructor instructor) {
        if (capacity < 0) {
            throw new InvalidSeminarCapacityRuntimeException(capacity);
        }
        this.semniarId = seminarId;
        this.course = course;
        this.capacity = capacity;
        this.instructor = instructor;
        seminarScheduleList = new ArrayList<Schedule>();
    }
    
    public String getSeminarId() {
        return semniarId;
    }
    
    public void setSeminarId(String seminarId) {
        this.semniarId = seminarId;
    }
    
    public Course getCourse() {
        return course;
    }
    
    public void setCourse(Course course) {
        this.course = course;
    }
    
    public int getCapacity() {
        return capacity;
    }
    
    public void setCapacity(int capacity) {
        if (capacity < 0) {
            throw new InvalidSeminarCapacityRuntimeException(capacity);
        }
        this.capacity = capacity;
    }
    
    public Instructor getInstructor() {
        return instructor;
    }
    
    public void setInstructor(Instructor instructor) {
        this.instructor = instructor;
    }
    
    public ArrayList<Schedule> getSeminarScheduleList() {
        ArrayList<Schedule> copyScheduleList = new ArrayList<Schedule>();
        for (Schedule schedule : seminarScheduleList) {
            copyScheduleList.add(schedule);
        }
        return copyScheduleList;
    }
    
    public void setSeminarScheduleList(ArrayList<Schedule> seminarScheduleList) {
        this.seminarScheduleList = new ArrayList<Schedule>();
        for (Schedule schedule : seminarScheduleList) {
            this.seminarScheduleList.add(schedule);
        }
    }
    
    public void addSchedule(Schedule addedSchedule) throws DuplicateScheduleException {
        if (containsOverlappingSchedule(addedSchedule)) {
            throw new DuplicateScheduleException(addedSchedule);
        }
        seminarScheduleList.add(addedSchedule);
    }
    
    public Schedule removeSchedule(Schedule removedSchedule) throws ScheduleNotFoundException {
      int removeIndex = -1;
      for (int i = 0; i < seminarScheduleList.size(); i++) {
          if (seminarScheduleList.get(i).scheduleIdEquals(removedSchedule)) {
              removeIndex = i;
              break;
          }
      }
      if (removeIndex == -1) {
          throw new ScheduleNotFoundException(removedSchedule);
      }
      return seminarScheduleList.remove(removeIndex);
    }
    
    public Schedule removeSchedule(String scheduleId) throws ScheduleIdNotFoundException {
        int removeIndex = -1;
        for (int i = 0; i < seminarScheduleList.size(); i++) {
            if (seminarScheduleList.get(i).scheduleIdEquals(scheduleId)) {
                removeIndex = i;
                break;
            }
        }
        if (removeIndex == -1) {
            throw new ScheduleIdNotFoundException(scheduleId);
        }
        return seminarScheduleList.remove(removeIndex);
      }
    
    public void show() {
        System.out.printf("Seminar ID: %s\nCourse Name: %s\nCourse Number: %s\nCapacity: %d\nInstructor: %s\n"
                + "Schedule:\n", semniarId, course.getName(), course.getCourseNumber(), capacity, instructor.getName());
        
        for (Schedule schedule : seminarScheduleList) {
            System.out.printf("    Day: %s  Start Time: %s  Duration: %d minutes  Location: %s\n", schedule.getDay(),
                    schedule.getStartTime().toString(), schedule.getDuration(), schedule.getLocation());
        }
        
        System.out.print("\n");
    }
    
    public boolean seminarIdEquals(Seminar other) {
        return this.getSeminarId().equals(other.getSeminarId());
    }
    
    public boolean seminarIdEquals(String seminarId) {
        return this.getSeminarId().equals(seminarId);
    }
    
    public boolean containsOverlappingSchedule(Schedule other) {
        for (Schedule schedule : seminarScheduleList) {
            if (schedule.doesOverlap(other)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public int compareTo(Object o) {
        Seminar s = (Seminar) o;
        return getSeminarId().compareTo(s.getSeminarId());
    }
}
