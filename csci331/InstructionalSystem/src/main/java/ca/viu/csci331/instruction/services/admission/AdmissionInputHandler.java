package ca.viu.csci331.instruction.services.admission;

import java.time.LocalDate;
import java.util.NoSuchElementException;
import java.util.Scanner;

public class AdmissionInputHandler {
    private static Scanner scanIn = new Scanner(System.in);
    
    public static final String getLineEntry() {
        String stringEntry = "";
        boolean invalidInput = true;

        do {
            try {
                stringEntry = scanIn.nextLine();
                invalidInput = false;
            } catch (java.util.NoSuchElementException e) {
                e.printStackTrace();
            }
        } while (invalidInput);
        return stringEntry.trim();
    }
    
    
    public static final int getIntEntry() {
        int intEntry = 0;
        try {
            intEntry = scanIn.nextInt();
        } catch (java.util.InputMismatchException e) {
            scanIn.nextLine();
            intEntry = -1;
        } catch (NoSuchElementException e) {
            e.printStackTrace();
        }
        return intEntry;
    }
    
    
    public static final int getYearIntEntry(String prompt) {
        int intEntry = 0;
        boolean invalidInput = true;
        System.out.print("\nPlease enter year (e.g. 2010)\n" + prompt);
        do {
            intEntry = getIntEntry();
            if (intEntry < 2000) {
                System.out.print("Year must be a 4 digit integer. Earliest accepted year is 2000\n" + prompt);
            } else {
                invalidInput = false;
            }
        } while (invalidInput);
        return intEntry;
    }
    
    
    public static final int getMonthIntEntry(String prompt) {
        int intEntry = 0;
        boolean invalidInput = true;
        System.out.print("\nPlease enter month as an integer (e.g. 3 = March)\n" + prompt);

        do {
            intEntry = getIntEntry();
            if (intEntry < 1 || intEntry > 12) {
                System.out.print("Month must be an integer between 1 and 12\n" + prompt);
            } else {
                invalidInput = false;
            }
        } while (invalidInput);
        return intEntry;
    }
    
    
    public static final int getDayIntEntry(String prompt, int year, int month) {
        int lengthOfMonth = LocalDate.of(year, month, 1).lengthOfMonth();
        int intEntry = 0;
        boolean invalidInput = true;
        System.out.print("\nPlease enter day as an integer (e.g. 21)\n" + prompt);

        do {
            intEntry = getIntEntry();
            if (intEntry < 1 || intEntry > lengthOfMonth) {
                System.out.print("Day must be an integer between 1 and " + lengthOfMonth + "\n" + prompt);
            } else {
                invalidInput = false;
            }
        } while (invalidInput);
        return intEntry;
    }
}
