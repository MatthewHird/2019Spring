package ca.viu.csci331.assignment1;

/**
 * Application entry point. In oder the application will:
 * Asks the user to enter the number of students that this application is going to process.     
 * a) Add the required number of students in the application.
 * b) Print student information of every student entered in the application (to STDOUT).
 * c) Search a student by name, and print out that student's information.
 * d) Search a student by id, and print out that student's information.
 * e) Order the students by gpa, in decending order.
 * f) Print student information in order of gpa, in decending order.
 * g) Remove the student with the lowest GPA from the application using removeById().
 * h) Print student information after removing the student with the lowest GPA.
 * 
 * @author Matthew Hird
 * @date Jan. 26, 2019
 */
public class Main {

    public static void main(String[] args) {
        ProcessStudents processStudents = new ProcessStudents(10);

        processStudents.addStudent(new Student("Jack Maxim", "289403729", 3.4445));
        processStudents.addStudent(new Student("Jane Dome", "903894392", 4.3333));
        processStudents.addStudent(new Student("Mitch Richmond", "772748212", 2.11));
        processStudents.addStudent(new Student("Al Slapper", "223223223", 1.0));
        processStudents.addStudent(new Student("Marky Mark", "123456789", 2.01));
        processStudents.addStudent(new Student("Debbie Flarps", "835199928", 3.0));
        processStudents.addStudent(new Student("Tim van Nerd", "656463626", 4.00));
        processStudents.addStudent(new Student("Quinn Terrot", "385489739", 0.5));
        processStudents.addStudent(new Student("Doug", "876321053", 2.75));
        processStudents.addStudent(new Student("Jack Foxx", "222222222", 3.629));
        
        processStudents.showStudents();
    }

}
