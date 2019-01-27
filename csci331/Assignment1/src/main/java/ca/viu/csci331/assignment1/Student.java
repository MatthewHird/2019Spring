package ca.viu.csci331.assignment1;

/**
 * Data class for storing basic student information.
 * 
 * @author Matthew Hird
 * @date Jan. 26, 2019
 */
public class Student {
    private String name;
    private String id;
    private double gpa;
    
    public Student(String name, String id, double gpa) {
        this.name = name;
        this.id = id;
        this.gpa = gpa;
    }
    
    public String getName() {
        return name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    
    public String getId() {
        return id;
    }
    
    public void setId(String id) {
        this.id = id;
    }
    
    public double getGpa() {
        return gpa;
    }
    
    public void setGpa(double gpa) {
        this.gpa = gpa;
    }
    
    /**
     * Prints name, id and gpa to STDOUT in a human readible format with headers.
     */
    public void show() {
        System.out.printf("Student Name: %s\nStudent ID:   %9s  GPA: %4.2f\n", name, id, gpa);
    }
}
