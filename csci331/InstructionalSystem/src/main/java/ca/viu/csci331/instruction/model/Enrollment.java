package ca.viu.csci331.instruction.model;

/**
 * Represents a student being enrolled in a seminar. Stores a <Student>, a <Seminar>, and a <double> grade.
 * @author Matthew Hird
 * @date Feb. 12, 2019
 */
public class Enrollment {
    private Student student;
    private Seminar seminar;
    private double grade;
    
    public Enrollment(Student student, Seminar seminar) {
        this.student = student;
        this.seminar = seminar;
        grade = 0;
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
    
    public void show() {
        System.out.printf("Student ID: %s\nStudent Name: %s\nSeminar ID: %s\n"
                + "Course Name: %s\nCourse Number: %s\nGrade: %.1f%%\n\n", 
                getStudent().getStudentId(), getStudent().getName(), 
                getSeminar().getSeminarId(), getSeminar().getCourse().getName(), 
                getSeminar().getCourse().getCourseNumber(), getGrade());
    }
    
    @Override
    public String toString() {
        return String.format("%s%s%f\n", getStudent().toString(), 
                getSeminar().toString(), getGrade());
    }
    
//    public boolean equals(Enrollment other) {
//        if (this.getStudent().getStudentId().equals(other.getStudent().getStudentId())
//                && this.getSeminar().getSeminarId().equals(other.getSeminar().getSeminarId())) {
//            return true;
//        }
//        return false;
//    }
}
