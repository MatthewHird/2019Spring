package ca.viu.csci331.assignment1;

import java.util.Scanner;

/**
 * Application entry point. In order the application will:
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
 * @date Feb. 3, 2019
 */
public class Main {

    public static void main(String[] args) {
        int studentCount = 0;
        boolean invalidInput = true;
        System.out.print("\nPlease enter the number of student you would like the system to process\n>>>  ");

        Scanner scanIn = new Scanner(System.in);
        do {
            try {
                studentCount = scanIn.nextInt();
                
                if (studentCount > 0) {
                    invalidInput = false;
                } else {
                    System.out.print("\nInvalid value entered:\nThe number of students must be a positive integer\n>>>  ");
                }
            } catch (java.util.InputMismatchException e) {
                
                scanIn.nextLine();
                System.out.print("\nInvalid value entered:\nThe number of students must be a positive integer\n>>>  ");
            }
        } while (invalidInput);
        
        ProcessStudents processStudents = new ProcessStudents(studentCount);
        
        String[] firstNamePool = 
            {"Olivia", "Emily", "Isla", "Sophie", "Amelia", "Jessica", "Ava", "Ella", "Charlotte", "Aria", "Lucy", "Lily", 
             "Grace", "Freya", "Ellie", "Evie", "Sophia", "Harper", "Hannah", "Millie", "Eilidh", "Chloe", "Mia", "Emma", 
             "Eva", "Anna", "Orla", "Ruby", "Poppy", "Maisie", "Jack", "Oliver", "James", "Lewis", "Logan", "Noah", "Harris", 
             "Alexander", "Leo", "Harry", "Alfie", "Finlay", "Jacob", "Charlie", "Aaron", "Lucas", "Rory", "Mason", "Archie", 
             "Thomas", "Daniel", "Adam", "Cameron", "Max", "Finn", "Ethan", "Matthew", "Theo", "Nathan", "Oscar"};
        String[] lastNamePool = 
            {"Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia", "Miller", "Davis", "Rodriguez", "Martinez", "Hernandez", 
             "Lopez", "Gonzalez", "Wilson", "Anderson", "Thomas", "Taylor", "Moore", "Jackson", "Martin", "Lee", "Perez", 
             "Thompson", "White", "Harris", "Sanchez", "Clark", "Ramirez", "Lewis", "Robinson", "Walker", "Young", "Allen", 
             "King", "Wright", "Scott", "Torres", "Nguyen", "Hill", "Flores", "Green", "Adams", "Nelson", "Baker", "Hall", 
             "Rivera", "Campbell", "Mitchell", "Carter", "Roberts", "Gomez", "Phillips", "Evans", "Turner", "Diaz", "Parker", 
             "Cruz", "Edwards", "Collins", "Reyes"};
        
        String searchName = "";
        String searchId = "";
        
        for (int i = 0; i < studentCount; i++) {
            String name = firstNamePool[(int) Math.floor(Math.random() * firstNamePool.length)] + " " 
                    + lastNamePool[(int) Math.floor(Math.random() * lastNamePool.length)];
            String id = "";
            for (int j = 0; j < 9; j++) {
                id = id.concat(Integer.toString((int) Math.floor(Math.random() * 10)));
            }
            double gpa = Math.random() * 4.333;
            
            processStudents.addStudent(new Student(name, id, gpa));
            
            if (i == (int) Math.floor(studentCount * 0.333)) {
                searchName = name;
            }
            if (i == (int) Math.floor(studentCount * 0.666)) {
                searchId = id;
            }
        }
        
        System.out.printf("\nDisplaying list of %d student%s added to the system\n\n", studentCount, (studentCount > 1)?"s":"");
        processStudents.showStudents();
        
        System.out.printf("Searching for student named \"%s\"\n\n", searchName);
        processStudents.searchByName(searchName).show();
        System.out.print("\n-------------------\n\n");
        
        System.out.printf("Searching for student with ID \"%s\"\n\n", searchId);
        processStudents.searchById(searchId).show();
        System.out.print("\n-------------------\n\n");
        
        System.out.print("Ranking and displaying students by GPA in descending order\n\n");
        processStudents.rankStudents();
        processStudents.showStudents();
        
        System.out.print("Removing student with the lowest GPA\n\n");
        processStudents.removeById(processStudents.lastStudent().getId());
        processStudents.showStudents();
        
        scanIn.close();
    }
}
