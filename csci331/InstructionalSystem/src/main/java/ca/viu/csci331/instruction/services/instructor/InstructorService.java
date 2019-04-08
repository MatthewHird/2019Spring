package ca.viu.csci331.instruction.services.instructor;

import java.util.ArrayList;
import java.util.Hashtable;

import ca.viu.csci331.instruction.exception.EnrollmentNotFoundException;
import ca.viu.csci331.instruction.model.Enrollment;
import ca.viu.csci331.instruction.model.Schedule;
import ca.viu.csci331.instruction.model.Seminar;
import ca.viu.csci331.instruction.model.Student;
import ca.viu.csci331.instruction.services.student.EnrollmentList;

public class InstructorService {
    private EnrollmentList enrollmentList;
    
    public InstructorService(EnrollmentList enrollmentList) {
        this.enrollmentList = enrollmentList;
    }
    
    public ArrayList<Enrollment> getAllEnrollments() {
        return enrollmentList.getAllEnrollments();
    }
    
    public ArrayList<Seminar> searchSeminarsByInstructorId(String instructorId) {
        ArrayList<Enrollment> enrollments = enrollmentList.searchByInstructorId(instructorId);
        Hashtable<String,Seminar> seminarMap = new Hashtable<>();
        for (Enrollment enrollment : enrollments) {
            Seminar curSem = enrollment.getSeminar();
            seminarMap.putIfAbsent(curSem.getSeminarId(), curSem);
        }
        ArrayList<Seminar> seminars = new ArrayList<>(seminarMap.values());
        seminars.sort((Seminar o1, Seminar o2)->o1.compareTo(o2));
        return seminars;
    }
    
    public void submitStudentGrade(String studentId, String seminarId, double grade) throws EnrollmentNotFoundException {
        Enrollment enrollment = enrollmentList.searchByStuIdSemId(studentId, seminarId);
        enrollment.setGrade(grade);
    }
    
    public String getCurrentTimetable(String instructorId) {
        String timeTable = String.format("Current Time Table for Instructor with ID #%s\n\n", instructorId);
        ArrayList<Enrollment> enrollments = enrollmentList.searchIncompleteByInstructorId(instructorId);
        Hashtable<String,Seminar> seminarMap = new Hashtable<>();
        for (Enrollment enrollment : enrollments) {
            Seminar curSem = enrollment.getSeminar();
            seminarMap.putIfAbsent(curSem.getSeminarId(), curSem);
        }
        ArrayList<Seminar> seminars = new ArrayList<>(seminarMap.values());
        seminars.sort((Seminar o1, Seminar o2)->o1.compareTo(o2));
        
        for (Seminar seminar : seminars) {
            for (Schedule schedule : seminar.getSeminarScheduleList()) {
                timeTable += String.format("Course #: %s  Seminar ID: %s  Day: %s  Start Time: %s  End Time: %s  "
                        + "Building: %s  Room: %s\n\n", seminar.getCourse().getCourseNumber(), 
                        seminar.getSeminarId(), schedule.getDay(), schedule.getStartTime().toString(), 
                        schedule.getStartTime().plusMinutes(schedule.getDuration()).toString(),
                        schedule.getLocation().getBuildingNumber(), schedule.getLocation().getRoomNumber());
            }
        }
        return timeTable;
    }
    
    public ArrayList<Student> searchStudentsBySeminarId(String seminarId) {
        ArrayList<Enrollment> enrollments = enrollmentList.searchBySeminarId(seminarId);
        ArrayList<Student> students = new ArrayList<>();
        for (Enrollment enrollment : enrollments) {
            students.add(enrollment.getStudent());
        }
        students.sort((Student o1, Student o2)->o1.compareTo(o2));
        return students;
    }
}
















