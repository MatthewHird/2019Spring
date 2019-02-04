package ca.viu.csci331.assignment1;

import java.util.List;

import java.util.ArrayList;
import java.util.Collections;

/**
 * A class to store and manage <Student> objects. Can add students, remove students by id, search for 
 * students by name or id, search for the student with the best or worst gpa, display all student 
 * information, or reorder students based on gpa. 
 * 
 * @author Matthew Hird
 * @date Jan. 26, 2019
 */
public class ProcessStudents {
    private List<Student> students;
    
    /**
     * The primary class constructor.
     * 
     * @param studentCount The number of <Student> objects to be processed by the system (the capacity).
     */
    public ProcessStudents(int studentCount) {
        students = new ArrayList<Student>(studentCount);
    }
    
    /**
     * Stores a <Student> object in the system.
     * 
     * @param newStudent The <Student> to be added to the system.
     */
    public void addStudent(Student newStudent) {
        students.add(newStudent);
    }
    
    /**
     * Displays the student information of all the <Student> objects stored in the system. The information
     * in printed to STDOUT in a pretty, human-readable format that include headers. The students are printed
     * in the order they are currently stored in the system (the default is the order they are added to the system).
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
     * Sorts the order of <Student> objects in the system by GPA in descending order.
     */
    public void rankStudents() {
        Collections.sort(students, (s1, s2) -> Double.compare(s2.getGpa(), s1.getGpa()));
    }
    
    /**
     * Searchs for a <Student> by name.
     * 
     * @param studentName The name of the student to be retrieved.
     * @return The <Student> with the name studentName if it exists.
     *         Otherwise returns an empty <Student> (ie new Student("","",0)).
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
     * Searchs for a <Student> by student ID.
     * 
     * @param studentId The ID of the student to be retrieved.
     * @return The <Student> with the ID studentId if it exists.
     *         Otherwise returns an empty <Student> (ie new Student("","",0)).
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
     * Finds the student information of the <Student> with the highest GPA.
     * 
     * @return The <Student> with the highest gpa value. If multiple students are tied, 
     *         it returns the first one storwed in the system. If no students are currently 
     *         stored in the system, returns an empty <Student> (ie new Student("","",0)).
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
     * Finds the student information of the <Student> with the lowest GPA.
     * 
     * @return The <Student> with the lowest gpa value. If multiple students are tied, 
     *         it returns the last one storwed in the system. If no students are currently 
     *         stored in the system, returns an empty <Student> (ie new Student("","",0)).
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
     * Removes all <Students> from the system with the specified ID.
     * 
     * @param studentId The student id of the <Student> to be removed from the system.
     */
    public void removeById(String studentId) {
        students.removeIf(s -> (s.getId().equals(studentId)));
    }
}
