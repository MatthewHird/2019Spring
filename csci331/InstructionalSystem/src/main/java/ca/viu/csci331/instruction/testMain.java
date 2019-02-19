package ca.viu.csci331.instruction;

import ca.viu.csci331.instruction.model.AdmittedStudents;
import ca.viu.csci331.instruction.model.Course;
import ca.viu.csci331.instruction.model.Enrollment;
import ca.viu.csci331.instruction.model.Enrollments;
import ca.viu.csci331.instruction.model.Instructor;
import ca.viu.csci331.instruction.model.Instructors;
import ca.viu.csci331.instruction.model.OfferedCourses;
import ca.viu.csci331.instruction.model.Schedule;
import ca.viu.csci331.instruction.model.Schedules;
import ca.viu.csci331.instruction.model.Seminar;
import ca.viu.csci331.instruction.model.Seminars;
import ca.viu.csci331.instruction.model.Student;

/**
 * Simple runner class to demo the functionality of all of the classes' constructors and show functions.
 * @author Matthew Hird
 * @date Feb. 12, 2019
 */
public class testMain {

    public static void main(String[] args) {
        Course a1 = new Course("Object Oriented Programming","CSCI 331",3,"OO progamming in Java.");
        Instructor a2 = new Instructor("John Wayne","123456","jw@email.org");
        Schedule a3 = new Schedule("Monday",2,3,4,"B200 R105");
        Student a4 = new Student("Jane Doe","123456789","jd@email.org");
        Seminar a5 = new Seminar("mySem",a1,5,a2);
        
        a5.addValidSchedule(a3);
        Enrollment a6 = new Enrollment(a4, a5);
        
        AdmittedStudents a7 = new AdmittedStudents(5);
        Enrollments a8 = new Enrollments(5);
        Instructors a9 = new Instructors(5);
        OfferedCourses a10 = new OfferedCourses(5);
        Schedules a11 = new Schedules(5);
        Seminars a12 = new Seminars(5);
        
        a7.admit(a4);
        a8.enroll(a6);
        a9.hire(a2);
        a10.offer(a1);
        a11.add(a3);
        a12.add(a5);
        
        a1.show();
        a2.show();
        a3.show();
        a4.show();
        a5.show();
        a6.show();
        
        a7.showAll();
        a8.showAll();
        a9.showAll();
        a10.showAll();
        a11.showAll();
        a12.showAll();
    }

}
