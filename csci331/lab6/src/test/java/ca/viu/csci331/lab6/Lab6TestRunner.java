package ca.viu.csci331.lab6;

import org.junit.runner.JUnitCore;

public class Lab6TestRunner {
    public static void main(String[] args) {
        System.out.print("Running Lab6 unit tests:\n\n");
        JUnitCore.main("ca.viu.csci331.lab6.RationalNumberListManagerTests",
                "ca.viu.csci331.lab6.RatNumBubbleSorterTests",
                "ca.viu.csci331.lab6.RatNumQuickSorterTests");
    }
}
