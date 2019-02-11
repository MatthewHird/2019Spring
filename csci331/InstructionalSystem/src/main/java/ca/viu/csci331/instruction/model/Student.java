package ca.viu.csci331.instruction.model;

public class Student {
    private String name;
    private String studentId;
    private String email;

    public Student(String name, String studentId, String email) {
        this.name = name;
        this.studentId = studentId;
        this.email = email;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getStudentId() {
        return studentId;
    }

    public void setStudentId(String studentId) {
        this.studentId = studentId;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public void show() {
        System.out.printf("Student Name: %s\nStudent ID:   %9s\nEmail Address: %s\n\n", name, studentId, email);
    }
}
