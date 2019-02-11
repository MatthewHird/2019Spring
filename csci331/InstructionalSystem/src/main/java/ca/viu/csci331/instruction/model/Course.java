package ca.viu.csci331.instruction.model;

public class Course {
    private String name;
    private String courseNumber;
    private double credit;
    private String description;
    
    public Course (String name, String courseNumber, double credit, String description) {
        this.name = name;
        this.courseNumber = courseNumber;
        this.credit = credit;
        this.description =description;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getCourseNumber() {
        return courseNumber;
    }

    public void setCourseNumber(String courseNumber) {
        this.courseNumber = courseNumber;
    }

    public double getCredit() {
        return credit;
    }

    public void setCredit(double credit) {
        this.credit = credit;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public void show() {
        System.out.printf("Course Name: %s\nCourse Number: %8s\nCredits: %4.2f\nDescription: %s\n\n", name, courseNumber, credit, description);
    }
}
