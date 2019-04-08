package ca.viu.csci331.instruction.model;

/**
 * Represents a student being enrolled in a seminar. Stores a <Student>, a <Seminar>, and a <double> grade.
 * @author Matthew Hird
 * @date Feb. 12, 2019
 */
public class Enrollment implements Comparable<Object> {
    private Student student;
    private Seminar seminar;
    private double grade;
    private boolean complete;
    
    public Enrollment(Student student, Seminar seminar) {
        this.student = student;
        this.seminar = seminar;
        grade = 0;
        complete = false;
    }

    public Student getStudent() {
        return student;
    }

    public void setStudent(Student student) {
        this.student = student;
    }

    public Seminar getSeminar() {
        return seminar;
    }

    public void setSeminar(Seminar seminar) {
        this.seminar = seminar;
    }

    public double getGrade() {
        return grade;
    }

    public void setGrade(double grade) {
        this.grade = grade;
    }
    
    public boolean getComplete() {
        return complete;
    }
    
    public void setComplete(boolean complete) {
        this.complete = complete;
    }
    
    public void show() {
        String status;
        if (getComplete()) {
            status = "Completed";
        } else {
            status = "Incomplete";
        }
        System.out.printf("Student ID: %s\nStudent Name: %s\nSeminar ID: %s\n"
                + "Course Name: %s\nCourse Number: %s\nGrade: %.1f%%\nStatus: %s\n\n", 
                getStudent().getStudentId(), getStudent().getName(), 
                getSeminar().getSeminarId(), getSeminar().getCourse().getName(), 
                getSeminar().getCourse().getCourseNumber(), getGrade(), status);
    }
    
    @Override
    public String toString() {
        return String.format("%s%s%f\n%s\n", getStudent().toString(), 
                getSeminar().toString(), getGrade(), getComplete());
    }
    
    public boolean equalStuIdSemId(Enrollment other) {
        if (this.getStudent().getStudentId().equals(other.getStudent().getStudentId())
                && this.getSeminar().getSeminarId().equals(other.getSeminar().getSeminarId())) {
            return true;
        }
        return false;
    }
    
    public boolean equalStuIdSemId(String studentId, String seminarId) {
        if (this.getStudent().getStudentId().equals(studentId)
                && this.getSeminar().getSeminarId().equals(seminarId)) {
            return true;
        }
        return false;
    }

    @Override
    public int compareTo(Object o) {
        Enrollment s = (Enrollment) o;
        int result = this.getSeminar().compareTo(s.getSeminar());
        if (result == 0) {
            result = this.getStudent().compareTo(s.getStudent());
        }
        return result;
    }
}
