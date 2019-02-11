package ca.viu.csci331.instruction.model;

public class testMain {

    public static void main(String[] args) {
        Course a1 = new Course("1","2",3,"4");
        Instructor a2 = new Instructor("1","2","3");
        Schedule a3 = new Schedule("1",2,3,4,"5");
        Student a4 = new Student("1","2","3");
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
