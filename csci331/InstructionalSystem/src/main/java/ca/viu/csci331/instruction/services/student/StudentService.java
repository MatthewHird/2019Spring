package ca.viu.csci331.instruction.services.student;

import java.util.ArrayList;

import ca.viu.csci331.instruction.exception.DuplicateEnrollmentException;
import ca.viu.csci331.instruction.exception.EnrollmentNotFoundException;
import ca.viu.csci331.instruction.model.Enrollment;
import ca.viu.csci331.instruction.model.Schedule;
import ca.viu.csci331.instruction.model.Seminar;
import ca.viu.csci331.instruction.model.Student;

public class StudentService {
    private EnrollmentList enrollmentList;
    
    public StudentService() {
        enrollmentList = new EnrollmentList();
    }
    
    public StudentService(EnrollmentList enrollmentList) {
        this.enrollmentList = enrollmentList;
    }
    
    public ArrayList<Enrollment> getAllEnrollments() {
        return enrollmentList.getAllEnrollments();
    }
    
    public boolean enroll(Student student, Seminar seminar) throws DuplicateEnrollmentException {
        if (enrollmentList.containsStuIdSemId(student.getStudentId(), seminar.getSeminarId())) {
            throw new DuplicateEnrollmentException(student.getStudentId(), seminar.getSeminarId());
        }
        enrollmentList.add(student, seminar);
        return true;
    }
    
    public Enrollment withdraw(String studentId, String seminarId) throws EnrollmentNotFoundException {
        return enrollmentList.remove(studentId, seminarId);
    }
    
    public ArrayList<Seminar> getEnrolledSeminarsByStudentId(String studentId) {
        ArrayList<Enrollment> enrollments = enrollmentList.searchByStudentId(studentId);
        ArrayList<Seminar> seminars = new ArrayList<>();
        for (Enrollment enrollment : enrollments) {
            seminars.add(enrollment.getSeminar());
        }
        return seminars;
    }
    
    public String getCurrentTimeTableByStudentId(String studentId) {
        String timeTable = String.format("Current Time Table for Student with ID #%s\n\n", studentId);
        ArrayList<Enrollment> enrollments = enrollmentList.searchIncompleteByStudentId(studentId);
        for (Enrollment enrollment : enrollments) {
            Seminar seminar = enrollment.getSeminar();
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
    
    public String getTranscriptOfCompletedByStudentId(String studentId) {
        String transcript = String.format("Transcript of Completed Seminars for Student with ID #%s\n\n", studentId);
        ArrayList<Enrollment> enrollments = enrollmentList.searchCompletedByStudentId(studentId);
        for (Enrollment enrollment : enrollments) {
            Seminar seminar = enrollment.getSeminar();
            transcript += String.format("Course #: %s  Course Name: %s  Grade: %f%%  Credits: %f\n\n", 
                    seminar.getCourse().getCourseNumber(), seminar.getCourse().getName(),
                    enrollment.getGrade(), seminar.getCourse().getCredit());
        }
        return transcript;
    }
}
