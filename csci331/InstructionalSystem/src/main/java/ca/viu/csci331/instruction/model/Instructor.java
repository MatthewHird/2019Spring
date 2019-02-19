package ca.viu.csci331.instruction.model;

/**
 * A data class to store information on a university instructor.
 * @author Matthew Hird
 * @date Feb. 12, 2019
 */
public class Instructor {
    private String name;
    private String instructorId;
    private String email;
    
    public Instructor(String name, String instructorId, String email) {
        this.name = name;
        this.instructorId = instructorId;
        this.email = email;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getInstructorId() {
        return instructorId;
    }

    public void setInstructorId(String instructorId) {
        this.instructorId = instructorId;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }
    
    public void show() {
        System.out.printf("Instructor Name: %s\nInstructor ID: %s\nEmail Address: %s\n\n", name, instructorId, email);
    }
}
