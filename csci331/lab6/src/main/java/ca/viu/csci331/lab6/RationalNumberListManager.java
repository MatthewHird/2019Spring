package ca.viu.csci331.lab6;

import java.util.ArrayList;
import java.util.List;

public class RationalNumberListManager {

    public static void reduceAll(List<RationalNumber> ratNumList) {
        for (RationalNumber rationalNumber : ratNumList) {
            rationalNumber.reduce();
        }
    }
    
    public static ArrayList<RationalNumber> copyList(List<RationalNumber> ratNumList) {
        ArrayList<RationalNumber> listCopy = new ArrayList<>();
        for (RationalNumber rationalNumber : ratNumList) {
            listCopy.add(new RationalNumber(rationalNumber));
        }
        return listCopy;
    }
    
    public static ArrayList<RationalNumber> copyAndReduceAll(List<RationalNumber> ratNumList) {
        ArrayList<RationalNumber> listCopy = copyList(ratNumList);
        reduceAll(listCopy);
        return listCopy;
    }
    
    public static void sort(ArrayList<RationalNumber> ratNumList, boolean quickSort, boolean descending) {
        RatNumSorter sorter;
        if (quickSort) {
            sorter = new RatNumQuickSorter();
        } else {
            sorter = new RatNumBubbleSorter();
        }
        
        if (descending) {
            sorter.sortDescending(ratNumList);
        } else {
            sorter.sortAscending(ratNumList);
        }
    }
}
