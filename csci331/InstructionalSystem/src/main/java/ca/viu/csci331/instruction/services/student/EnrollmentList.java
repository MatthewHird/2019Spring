package ca.viu.csci331.instruction.services.student;

import java.util.ArrayList;
import java.util.List;

import ca.viu.csci331.instruction.exception.EnrollmentNotFoundException;
import ca.viu.csci331.instruction.model.Enrollment;
import ca.viu.csci331.instruction.model.Seminar;
import ca.viu.csci331.instruction.model.Student;

public class EnrollmentList {
    private List<Enrollment> enrollmentList;
    
    public EnrollmentList() {
        enrollmentList = new ArrayList<>();
    }
    
    public int getEnrollmentCount() {
        return enrollmentList.size();
    }
    
    public void add(Student student, Seminar seminar) {
        enrollmentList.add(new Enrollment(student, seminar));
    }
    
    public void add(Enrollment enrollment) {
        enrollmentList.add(enrollment);
    }
    
    public Enrollment remove(String studentId, String seminarId) throws EnrollmentNotFoundException {
        int removeIndex = -1;
        for (int i = 0; i < enrollmentList.size(); i++) {
            if (enrollmentList.get(i).equalStuIdSemId(studentId, seminarId)) {
                removeIndex = i;
                break;
            }
        }
        if (removeIndex == -1) {
            throw new EnrollmentNotFoundException(studentId, seminarId);
        }
        return enrollmentList.remove(removeIndex);
    }
    
    public Enrollment searchByStuIdSemId(String studentId, String seminarId) throws EnrollmentNotFoundException {
        for (Enrollment enrollment : enrollmentList) {
            if (enrollment.equalStuIdSemId(studentId, seminarId)) {
                return enrollment;
            }
        }
        throw new EnrollmentNotFoundException(studentId, seminarId);
    }
    
    public ArrayList<Enrollment> searchByStudentId(String studentId) {
        ArrayList<Enrollment> searchedEnrollments = new ArrayList<>();
        for (Enrollment enrollment : enrollmentList) {
            if (enrollment.getStudent().getStudentId().equals(studentId)) {
                searchedEnrollments.add(enrollment);
            }
        }
        return searchedEnrollments;
    }
    
    public ArrayList<Enrollment> searchCompletedByStudentId(String studentId) {
        ArrayList<Enrollment> searchedEnrollments = new ArrayList<>();
        for (Enrollment enrollment : enrollmentList) {
            if (enrollment.getStudent().getStudentId().equals(studentId)
                    && enrollment.getComplete()) {
                searchedEnrollments.add(enrollment);
            }
        }
        return searchedEnrollments;
    }
    
    public ArrayList<Enrollment> searchIncompleteByStudentId(String studentId) {
        ArrayList<Enrollment> searchedEnrollments = new ArrayList<>();
        for (Enrollment enrollment : enrollmentList) {
            if (enrollment.getStudent().getStudentId().equals(studentId)
                    && !enrollment.getComplete()) {
                searchedEnrollments.add(enrollment);
            }
        }
        return searchedEnrollments;
    }
    
    public ArrayList<Enrollment> searchByInstructorId(String instructorId) {
        ArrayList<Enrollment> searchedEnrollments = new ArrayList<>();
        for (Enrollment enrollment : enrollmentList) {
            if (enrollment.getSeminar().getInstructor().getInstructorId().equals(instructorId)) {
                searchedEnrollments.add(enrollment);
            }
        }
        return searchedEnrollments;
    }
    
    public ArrayList<Enrollment> searchIncompleteByInstructorId(String instructorId) {
        ArrayList<Enrollment> searchedEnrollments = new ArrayList<>();
        for (Enrollment enrollment : enrollmentList) {
            if (enrollment.getSeminar().getInstructor().getInstructorId().equals(instructorId)
                    && !enrollment.getComplete()) {
                searchedEnrollments.add(enrollment);
            }
        }
        return searchedEnrollments;
    }
    
    public ArrayList<Enrollment> searchBySeminarId(String seminarId) {
        ArrayList<Enrollment> searchedEnrollments = new ArrayList<>();
        for (Enrollment enrollment : enrollmentList) {
            if (enrollment.getSeminar().getSeminarId().equals(seminarId)) {
                searchedEnrollments.add(enrollment);
            }
        }
        return searchedEnrollments;
    }
    
    public ArrayList<Enrollment> getAllEnrollments() {
        ArrayList<Enrollment> allEnrollments = new ArrayList<>();
        for (Enrollment enrollment : enrollmentList) {
            allEnrollments.add(enrollment);
        }
        return allEnrollments;
    }
    
    public boolean containsStuIdSemId(String studentId, String seminarId) {
        for (Enrollment enrollment : enrollmentList) {
            if (enrollment.equalStuIdSemId(studentId, seminarId)) {
                return true;
            }
        }
        return false;
    }
}
