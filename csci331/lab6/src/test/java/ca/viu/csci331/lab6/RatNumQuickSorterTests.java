package ca.viu.csci331.lab6;

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;

import org.junit.Test;

public class RatNumQuickSorterTests {
    @Test
    public void testSortAscending() {
        ArrayList<RationalNumber> testList = new ArrayList<>();
        testList.add(new RationalNumber(2, 8));
        testList.add(new RationalNumber(3, 2));
        testList.add(new RationalNumber(5, 8139));
        testList.add(new RationalNumber(16, 4));
        ArrayList<RationalNumber> expected = new ArrayList<>();
        expected.add(new RationalNumber(5, 8139));
        expected.add(new RationalNumber(2, 8));
        expected.add(new RationalNumber(3, 2));
        expected.add(new RationalNumber(16, 4));
        
        RatNumQuickSorter sorter = new RatNumQuickSorter();
        sorter.sortAscending(testList);
        for (int i = 0; i < testList.size(); i++) {
            assertTrue(expected.get(i).equalFields(testList.get(i)));
        }
    }
    
    @Test
    public void testSortDescending() {
        ArrayList<RationalNumber> testList = new ArrayList<>();
        testList.add(new RationalNumber(2, 8));
        testList.add(new RationalNumber(3, 2));
        testList.add(new RationalNumber(5, 8139));
        testList.add(new RationalNumber(16, 4));
        ArrayList<RationalNumber> expected = new ArrayList<>();
        expected.add(new RationalNumber(16, 4));
        expected.add(new RationalNumber(3, 2));
        expected.add(new RationalNumber(2, 8));
        expected.add(new RationalNumber(5, 8139));
        
        RatNumQuickSorter sorter = new RatNumQuickSorter();
        sorter.sortDescending(testList);
        for (int i = 0; i < testList.size(); i++) {
            assertTrue(expected.get(i).equalFields(testList.get(i)));
        }
    }
}
