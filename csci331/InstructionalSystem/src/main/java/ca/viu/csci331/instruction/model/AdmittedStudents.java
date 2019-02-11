package ca.viu.csci331.instruction.model;

import java.util.ArrayList;
import java.util.List;

public class AdmittedStudents {
    private List<Student> students;
    private int capacity;
    private int studentCount;
    
    public AdmittedStudents(int capacity) {
        this.capacity = capacity;
        studentCount = 0;
        students = new ArrayList<Student>();
    }
    
    public void admit(Student admittedStudent) {
        if (studentCount < capacity) {
            if (!containsId(admittedStudent.getStudentId())) {
                students.add(admittedStudent);
                studentCount++;
            } else {
                System.out.print("Could not admit student: Student with same ID already in system\n\n");
            }
        } else {
            System.out.print("Could not admit student: Admissions list at capacity\n\n");
        }
    }
    
    public void cancel(Student cancelledStudent) {
        students.removeIf(s -> (s.getStudentId().equals(cancelledStudent.getStudentId())));
        studentCount = students.size();
    }
    
    public void showAll() {
        System.out.print("\n~~~~~~~~~~~~~~~~~~~~\nList of All Students\n~~~~~~~~~~~~~~~~~~~~\n\n");
        for (Student student : students) {
            student.show();
        }
        System.out.print("~~~~~~~~~~~~~~~~~~~~\n\n");
    }
    
    public Student searchByName(String studentName) {
        for (Student student : students) {
            if (student.getName().equals(studentName)) {
                return student;
            }
        }
        return new Student("STUDENTNOTFOUND", "STUDENTNOTFOUND", "");
    }
    
    public Student searchById(String studentId) {
        for (Student student : students) {
            if (student.getStudentId().equals(studentId)) {
                return student;
            }
        }
        return new Student("STUDENTNOTFOUND", "STUDENTNOTFOUND", "");
    }
    
    private boolean containsId(String studentId) {
        for (Student student : students) {
            if (student.getStudentId().equals(studentId)) {
                return true;
            }
        }
        return false;
    }
}
