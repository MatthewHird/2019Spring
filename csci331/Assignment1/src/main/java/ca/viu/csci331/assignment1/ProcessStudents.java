package ca.viu.csci331.assignment1;

import java.util.List;

import java.util.ArrayList;
import java.util.Collections;

/**
 * 
 * @author Matthew Hird
 * @date Jan. 26, 2019
 */
public class ProcessStudents {
    private List<Student> students;
    
    /**
     * 
     * @param studentCount
     */
    public ProcessStudents(int studentCount) {
        students = new ArrayList<Student>(studentCount);
    }
    
    /**
     * 
     * @param newStudent
     */
    public void addStudent(Student newStudent) {
        students.add(newStudent);
    }
    
    /**
     * 
     */
    public void showStudents() {
        System.out.print("-------------------\nStudent Information\n-------------------\n\n");
        for (Student student: students) {
            student.show();
            System.out.print("\n");
        }
        System.out.print("-------------------\n\n");
    }
    
    /**
     * 
     */
    public void rankStudents() {
        Collections.sort(students, (s1, s2) -> Double.compare(s2.getGpa(), s1.getGpa()));
    }
    
    /**
     * 
     * @param studentName
     * @return
     */
    public Student searchByName(String studentName) {
        for (Student student : students) {
            if (student.getName().contentEquals(studentName)) {
                return student;
            }
        }
        
        return new Student("","",0);
    }
    
    /**
     * 
     * @param studentId
     * @return
     */
    public Student searchById(String studentId) {
        for (Student student : students) {
            if (student.getId().equals(studentId)) {
                return student;
            }
        }
        return new Student("", "", 0);
    }
    
    /**
     * 
     * @return
     */
    public Student topStudent() {
        if (students.size() == 0) {
            return new Student("", "", 0);
        }
        
        Student topStu = students.get(0);
        
        for (Student student : students) {
            if (student.getGpa() > topStu.getGpa()) {
                topStu = student;
            }
        }
        
        return topStu;
    }
    
    /**
     * 
     * @return
     */
    public Student lastStudent() {
        if (students.size() == 0) {
            return new Student("", "", 0);
        }
        
        Student botStu = students.get(0);
        
        for (Student student : students) {
            if (student.getGpa() <= botStu.getGpa()) {
                botStu = student;
            }
        }
        
        return botStu;
    }
    
    /**
     * 
     * @param studentId
     */
    public void removeById(String studentId) {
        students.removeIf(s -> (s.getId().equals(studentId)));
    }
}
