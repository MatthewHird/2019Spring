package ca.viu.csci331.lab6;

import java.util.ArrayList;
import java.util.Collections;

public class RatNumQuickSorter implements RatNumSorter {

    @Override
    public void sortAscending(ArrayList<RationalNumber> ratNumList) {
        sortAscending(ratNumList, 0, ratNumList.size() - 1);
    }
    
    private void sortAscending(ArrayList<RationalNumber> ratNumList, int start, int end) {
        if (start < end) {
            int index = partAscending(ratNumList, start, end);
            
            sortAscending(ratNumList, start, index - 1);
            sortAscending(ratNumList, index + 1, end);
        }
    }
    
    private int partAscending(ArrayList<RationalNumber> ratNumList, int start, int end) {
        RationalNumber pivot = ratNumList.get(end);
        int index = start;
        
        for (int j = start; j < end; j++) {
            if (ratNumList.get(j).toDouble() <= pivot.toDouble()) {
                Collections.swap(ratNumList, index, j);
                index++;
            }
        }
        
        Collections.swap(ratNumList, index, end);
        return index;
    }

    @Override
    public void sortDescending(ArrayList<RationalNumber> ratNumList) {
        sortDescending(ratNumList, 0, ratNumList.size() - 1);
    }
    
    private void sortDescending(ArrayList<RationalNumber> ratNumList, int start, int end) {
        if (start < end) {
            int index = partDescending(ratNumList, start, end);
            
            sortDescending(ratNumList, start, index - 1);
            sortDescending(ratNumList, index + 1, end);
        }
    }
    
    private int partDescending(ArrayList<RationalNumber> ratNumList, int start, int end) {
        RationalNumber pivot = ratNumList.get(end);
        int index = start;
        
        for (int j = start; j < end; j++) {
            if (ratNumList.get(j).toDouble() >= pivot.toDouble()) {
                Collections.swap(ratNumList, index, j);
                index++;
            }
        }
        
        Collections.swap(ratNumList, index, end);
        return index;
    }

}
