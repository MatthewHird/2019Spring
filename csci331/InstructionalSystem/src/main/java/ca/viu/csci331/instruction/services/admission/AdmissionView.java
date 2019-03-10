package ca.viu.csci331.instruction.services.admission;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Scanner;

import ca.viu.csci331.instruction.exception.DuplicateStudentAdmissionException;
import ca.viu.csci331.instruction.exception.StudentIdNotFoundException;
import ca.viu.csci331.instruction.model.Student;
import ca.viu.csci331.instruction.model.StudentAdmission;
import ca.viu.csci331.instruction.persistence.admission.AdmissionPersistence;
import ca.viu.csci331.instruction.retrieval.admission.AdmissionRetrieval;

public class AdmissionView {
    private StudentAdmissionList admissionList;
    private final String PROMPT1 = ">>>  ";
    private final String PROMPT2 = " >>  ";
    private final String PROMPT3 = "  >  ";
    private final String LINEBREAK = "\n-------------------\n\n";
    private static Scanner scanIn = new Scanner(System.in);
    
    private final String MAINMENU = "Please enter one of the following commands:\n\n" 
            + " admit: Admit a new student\n"
            + "cancel: Cancel the admission of an admitted student\n"
            + "update: Update the admission information of an existing student\n"
            + "report: Print a report of student subsets\n"
            + "search: Search for students based on name, student ID or admission date\n"
            + "  quit: Exit the student admission service\n\n"; 
    
    public AdmissionView() {
        admissionList = new StudentAdmissionList();
    }
    
    
    public void runService() {
        menu();
    }
    
    
    private void menu() {
        load();

        System.out.print("\n**** Welcome to the Student Admission Service ****\n\n");

        printMainMenu();
        boolean terminate = false;
        while (!terminate) {
            boolean redisplayMenu = true;
            
            System.out.print(PROMPT1);
            String command = getLineEntry().toLowerCase();

            if (command.equals("admit")) {
                admitNewStudent();
            } else if (command.equals("cancel")) {
                cancelExistingStudent();
            } else if (command.equals("update")) {
                updateExistingStudent();
            } else if (command.equals("report")) {
                printStudentReport();
            } else if (command.equals("search")) {
                searchForStudent();
            } else if (command.equals("quit")) {
                exitService();
                terminate = true;
                redisplayMenu = false;
            } else {
                System.out.print("\nInvalid command: please try again\n");
                redisplayMenu = false;
            }

            if (redisplayMenu) {
                printMainMenu();
            }
        }
        
        
        save();
        
        System.out.print("\n**** Thank you for using the Student Admission Service ****\n\n");
    }
    
    
    private void printMainMenu() {
        System.out.print(MAINMENU);
    }
    
    
    private void load() {

    }
    
    
    private void save() {
        
    }
    
    
    public void admitNewStudent() {
        String studentName;
        String studentId;
        String studentEmail;
        boolean invalidInput = true;
        
        System.out.print("\nPlease enter the student's name\n" + PROMPT3);
        do {
            studentName = getLineEntry();
            if (studentName.equals("")) {
                System.out.print("Student name may not be left blank\n" + PROMPT3);
            } else {
                invalidInput = false;
            }
        } while (invalidInput);
        
        invalidInput = true;
        System.out.print("\nPlease enter the student's email\n" + PROMPT3);
        do {
            studentEmail = getLineEntry();
            if (studentEmail.equals("")) {
                System.out.print("Student email may not be left blank\n" + PROMPT3);
            } else {
                invalidInput = false;
            }
        } while (invalidInput);
        
        
        invalidInput = true;
        do {
            studentId = generateStudentId();
            if (!admissionList.containsStudentId(studentId)) {
                invalidInput = false;
            }
        } while (invalidInput);
        
        System.out.print("\n" + studentName + "'s new student ID is " + studentId + "\n");
        
        try {
            admissionList.add(new StudentAdmission(new Student(studentName, studentId, studentEmail)));
            System.out.print("\nStudent has successfully been admitted" + LINEBREAK);
        } catch (DuplicateStudentAdmissionException e) {
            System.out.print(e.getMessage());
            e.printStackTrace();
        }
    }
    
    
    public void cancelExistingStudent() {
        System.out.print("Please enter the student ID of the admitted student whose admission status\n"
                + "is to be updated to \"cancelled\" (enter 0 to return to main menu)\n" + PROMPT3);
        String enteredStudentId = getLineEntry();
        if (enteredStudentId.equals("0")) {
            System.out.print("\nRequest cancelled. Returning to main menu" + LINEBREAK);
        } else if (!admissionList.containsStudentId(enteredStudentId)) {
            System.out.print("\nNo student in the system has a student ID of" + enteredStudentId
                    + "\nReturning to main menu" + LINEBREAK);
        } else {
            try {
                admissionList.searchByStudentId(enteredStudentId).setAdmissionStatus("cancelled");
                System.out.print("\nAdmission status of student with ID " + enteredStudentId 
                        + " successfully updated to cancelled" + LINEBREAK);
            } catch (StudentIdNotFoundException e) {
                System.out.print(e.getMessage());
                e.printStackTrace();
            }
        }
    }
    
    
    public void updateExistingStudent() {
        System.out.print("Please enter the student ID of the student whose information is to be updated\n"
                + "(enter 0 to return to main menu)\n" + PROMPT3);
        String enteredStudentId = getLineEntry();
        if (enteredStudentId.equals("0")) {
            System.out.print("\nRequest cancelled. Returning to main menu" + LINEBREAK);
        } else if (!admissionList.containsStudentId(enteredStudentId)) {
            System.out.print("\nNo student in the system has a student ID of" + enteredStudentId
                    + "\nReturning to main menu" + LINEBREAK);
        } else {
            StudentAdmission studentData;
            boolean invalidInput;
            String command;
            
            try {
                studentData = admissionList.searchByStudentId(enteredStudentId);
            } catch (StudentIdNotFoundException e) {
                System.out.print(e.getMessage());
                e.printStackTrace();
                return;
            }
            
            System.out.print("Student name is currently set to:\n    " + studentData.getStudent().getName() 
                    + "\nWould you like to update it? (y or n)\n" + PROMPT2);
            
            invalidInput = true;
            do {
                command = getLineEntry().toLowerCase();
                if (command.equals("y")) {
                    System.out.print("\nPlease enter a new name\n" + PROMPT3);
                    String studentName;
                    
                    do {
                        studentName = getLineEntry();
                        if (studentName.equals("")) {
                            System.out.print("Student name may not be left blank\n" + PROMPT3);
                        } else {
                            studentData.getStudent().setName(studentName);
                            invalidInput = false;
                        }
                    } while (invalidInput);

                } else if (command.equals("n")) {
                    invalidInput = false;
                } else {
                    System.out.print("Invalid command: Please try again (y or n)" + PROMPT2);
                }
            } while (invalidInput);
            
 
            System.out.print("Student email is currently set to:\n    " + studentData.getStudent().getEmail()
                    + "\nWould you like to update it? (y or n)\n" + PROMPT2);
            
            invalidInput = true;
            do {
                command = getLineEntry().toLowerCase();
                if (command.equals("y")) {
                    System.out.print("\nPlease enter a new email address\n" + PROMPT3);
                    String studentEmail;
                    
                    do {
                        studentEmail = getLineEntry();
                        if (studentEmail.equals("")) {
                            System.out.print("Student email may not be left blank\n" + PROMPT3);
                        } else {
                            studentData.getStudent().setEmail(studentEmail);
                            invalidInput = false;
                        }
                    } while (invalidInput);

                } else if (command.equals("n")) {
                    invalidInput = false;
                } else {
                    System.out.print("Invalid command: Please try again (y or n)" + PROMPT2);
                }
            } while (invalidInput);
            
            
            System.out.print("Student admission status is currently set to:\n    " + studentData.getAdmissionStatus().toUpperCase()
                    + "\nWould you like to update it? (y or n)\n" + PROMPT2);
            
            invalidInput = true;
            do {
                command = getLineEntry().toLowerCase();
                if (command.equals("y")) {
                    System.out.print("\nPlease enter admission status \n(\"a\" for admitted and \"c\" for cancelled)\n" + PROMPT3);
                    String admissionStatus;
                    
                    do {
                        admissionStatus = getLineEntry();
                        if (admissionStatus.equals("a")) {
                            studentData.setAdmissionStatus("admitted");
                            invalidInput = false;
                        } else if (admissionStatus.equals("c")) {
                            studentData.setAdmissionStatus("cancelled");
                            invalidInput = false;
                        } else {
                            System.out.print("Admission status may not be left blank. Please try again\n"
                                    + "(\"a\" for admitted and \"c\" for cancelled)\n" + PROMPT3);
                        }
                    } while (invalidInput);

                } else if (command.equals("n")) {
                    invalidInput = false;
                } else {
                    System.out.print("Invalid command: Please try again (y or n)" + PROMPT2);
                }
            } while (invalidInput);
            
            
            System.out.print("Student admission date is currently set to:\n    " + studentData.getAdmissionDate().toString()
                    + " (yyyy-mm-dd)\nWould you like to update it? (y or n)\n" + PROMPT2);
            
            invalidInput = true;
            do {
                command = getLineEntry().toLowerCase();
                if (command.equals("y")) {
                    int year = getYearIntEntry();
                    int month = getMonthIntEntry();
                    int day = getDayIntEntry(year, month);
                    
                    studentData.setAdmissionDate(LocalDate.of(year, month, day));
                    invalidInput = false;
                } else if (command.equals("n")) {
                    invalidInput = false;
                } else {
                    System.out.print("Invalid command: Please try again (y or n)" + PROMPT2);
                }
            } while (invalidInput);
            
            
            System.out.print("\nStudent admission data has successfully been updated\nUpdated data is now:"
                    + "\n  Student Name:" + studentData.getStudent().getName() 
                    + "\n  Student ID: " + studentData.getStudent().getStudentId() 
                    + "\n  Student Email: " + studentData.getStudent().getEmail() 
                    + "\n  Admission Status: " + studentData.getAdmissionStatus().toUpperCase() 
                    + "\n  Admission Date: " + studentData.getAdmissionDate() + LINEBREAK);
        }
    }
    
    
    public void printStudentReport() {
        System.out.print(admissionList.allToString());
    }
    
    
    public void searchForStudent() {
        
    }
    
    
    public void exitService() {
        
    }
    
    
    private String getLineEntry() {
        String stringEntry = "";
        boolean invalidInput = true;

        do {
            try {
                stringEntry = scanIn.nextLine();
                invalidInput = false;
            } catch (java.util.InputMismatchException e) {
                scanIn.nextLine();
                System.out.print("\nBadInputSomehow\n");
            }
        } while (invalidInput);
        return stringEntry.trim();
    }
    
    
    private int getIntEntry() {
        int intEntry = 0;
        try {
            intEntry = scanIn.nextInt();
        } catch (java.util.InputMismatchException e) {
            scanIn.nextLine();
            intEntry = -1;
        }
        return intEntry;
    }
    
    
    private int getYearIntEntry() {
        int intEntry = 0;
        boolean invalidInput = true;
        System.out.print("\nPlease enter year (e.g. 2010)\n" + PROMPT3);
        do {
            intEntry = getIntEntry();
            if (intEntry < 2000) {
                System.out.print("Year must be a 4 digit integer. Earliest accepted year is 2000\n" + PROMPT3);
            } else {
                invalidInput = false;
            }
        } while (invalidInput);
        return intEntry;
    }
    
    
    private int getMonthIntEntry() {
        int intEntry = 0;
        boolean invalidInput = true;
        System.out.print("\nPlease enter month as an integer (e.g. 3 = March)\n" + PROMPT3);

        do {
            intEntry = getIntEntry();
            if (intEntry < 1 || intEntry > 12) {
                System.out.print("Month must be an integer between 1 and 12\n" + PROMPT3);
            } else {
                invalidInput = false;
            }
        } while (invalidInput);
        return intEntry;
    }
    
    
    private int getDayIntEntry(int year, int month) {
        int lengthOfMonth = LocalDate.of(year, month, 1).lengthOfMonth();
        int intEntry = 0;
        boolean invalidInput = true;
        System.out.print("\nPlease enter day as an integer (e.g. 21)\n" + PROMPT3);

        do {
            intEntry = getIntEntry();
            if (intEntry < 1 || intEntry > lengthOfMonth) {
                System.out.print("Day must be an integer between 1 and " + lengthOfMonth + "\n" + PROMPT3);
            } else {
                invalidInput = false;
            }
        } while (invalidInput);
        return intEntry;
    }
    
    
    private ArrayList<StudentAdmission> getAdmitted(ArrayList<StudentAdmission> inList) {
        ArrayList<StudentAdmission> outList = new ArrayList<StudentAdmission>();
        
        for (int i = 0; i < inList.size(); i++) {
            if (inList.get(i).getAdmissionStatus().contentEquals("admitted")) {
                outList.add(inList.get(i));
            }
        }
        return outList;
    }
    
    
    private ArrayList<StudentAdmission> getCancelled(ArrayList<StudentAdmission> inList) {
        ArrayList<StudentAdmission> outList = new ArrayList<StudentAdmission>();
        
        for (int i = 0; i < inList.size(); i++) {
            if (inList.get(i).getAdmissionStatus().contentEquals("cancelled")) {
                outList.add(inList.get(i));
            }
        }
        return outList;
    }
    
    
    private String generateStudentId() {
        int studentId = 0;
        
        do {
            studentId = (int) (Math.random() * 999999999);
        } while (studentId < 100000000);
        
        return Integer.toString(studentId);
    }
}
