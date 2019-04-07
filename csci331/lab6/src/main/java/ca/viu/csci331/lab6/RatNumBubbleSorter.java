package ca.viu.csci331.lab6;

import java.util.ArrayList;
import java.util.Collections;

public class RatNumBubbleSorter implements RatNumSorter {

    @Override
    public void sortAscending(ArrayList<RationalNumber> ratNumList) {
        int size = ratNumList.size();
        for (int i = 0; i < size - 1; i++) {
            for (int j = 0; j < size - i - 1; j++) {
                if (ratNumList.get(j).toDouble() > ratNumList.get(j + 1).toDouble()) {
                    Collections.swap(ratNumList,j , j + 1);
                }
            }
        }
    }

    @Override
    public void sortDescending(ArrayList<RationalNumber> ratNumList) {
        int size = ratNumList.size();
        for (int i = 0; i < size - 1; i++) {
            for (int j = 0; j < size - i - 1; j++) {
                if (ratNumList.get(j).toDouble() < ratNumList.get(j + 1).toDouble()) {
                    Collections.swap(ratNumList,j , j + 1);
                }
            }
        }
    }
}
