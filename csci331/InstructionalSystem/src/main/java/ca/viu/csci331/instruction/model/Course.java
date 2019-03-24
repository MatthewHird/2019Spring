package ca.viu.csci331.instruction.model;

/**
 * A data class to store information on a university course.
 * @author Matthew Hird
 * @date Feb. 12, 2019
 */
public class Course implements Comparable<Object> {
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
        System.out.printf("Course Name: %s\nCourse Number: %s\nCredits: %4.2f\nDescription: %s\n\n", 
                getName(), getCourseNumber(), getCredit(), getDescription());
    }
    
    public boolean memberValuesEqual(Course other) {
        if (this.getName().equals(other.getName())
                && this.getCourseNumber().equals(other.getCourseNumber())
                && this.getCredit() == other.getCredit()
                && this.getDescription().equals(other.getDescription())) {
            return true;
        }
        return false;
    }
    
    @Override
    public String toString() {
        return String.format("%s\n%s\n%f\n%s\n", 
                getName(), getCourseNumber(), getCredit(), getDescription());
    }
    
    @Override
    public int compareTo(Object o) {
        Course s = (Course) o;
        return this.getCourseNumber().compareTo(s.getCourseNumber());
    }
}
