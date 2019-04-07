package ca.viu.csci331.lab6;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;

import org.junit.Test;

public class RationalNumberListManagerTests {

    @Test
    public void testReduceAll() {
        ArrayList<RationalNumber> testList = new ArrayList<>();
        testList.add(new RationalNumber(2, 8));
        testList.add(new RationalNumber(3, 2));
        testList.add(new RationalNumber(5, 8139));
        testList.add(new RationalNumber(16, 4));
        ArrayList<RationalNumber> expected = new ArrayList<>();
        expected.add(new RationalNumber(1, 4));
        expected.add(new RationalNumber(3, 2));
        expected.add(new RationalNumber(5, 8139));
        expected.add(new RationalNumber(4, 1));
        
        RationalNumberListManager.reduceAll(testList);
        
        assertEquals(expected.size(), testList.size());
        
        for (int i = 0; i < expected.size(); i++) {
            assertTrue(expected.get(i).equalFields(testList.get(i)));
        }
    }
    
    @Test
    public void testCopyList() {
        ArrayList<RationalNumber> testList = new ArrayList<>();
        testList.add(new RationalNumber(2, 8));
        testList.add(new RationalNumber(3, 2));
        testList.add(new RationalNumber(5, 8139));
        testList.add(new RationalNumber(16, 4));
        ArrayList<RationalNumber> expected = new ArrayList<>();
        expected.add(new RationalNumber(2, 8));
        expected.add(new RationalNumber(3, 2));
        expected.add(new RationalNumber(5, 8139));
        expected.add(new RationalNumber(16, 4));
        
        ArrayList<RationalNumber> copyList = RationalNumberListManager.copyList(testList);
        
        assertEquals(copyList.size(), testList.size());
        assertEquals(expected.size(), testList.size());

        assertNotEquals(copyList, testList);
        for (int i = 0; i < testList.size(); i++) {
            assertTrue(expected.get(i).equalFields(testList.get(i)));
        }
        for (int i = 0; i < testList.size(); i++) {
            assertTrue(expected.get(i).equalFields(copyList.get(i)));
        }
    }
    
    @Test
    public void testCopyAndReduceAll() {
        ArrayList<RationalNumber> testList = new ArrayList<>();
        testList.add(new RationalNumber(2, 8));
        testList.add(new RationalNumber(3, 2));
        testList.add(new RationalNumber(5, 8139));
        testList.add(new RationalNumber(16, 4));
        ArrayList<RationalNumber> expected1 = new ArrayList<>();
        expected1.add(new RationalNumber(2, 8));
        expected1.add(new RationalNumber(3, 2));
        expected1.add(new RationalNumber(5, 8139));
        expected1.add(new RationalNumber(16, 4));
        
        ArrayList<RationalNumber> expected2 = new ArrayList<>();
        expected2.add(new RationalNumber(1, 4));
        expected2.add(new RationalNumber(3, 2));
        expected2.add(new RationalNumber(5, 8139));
        expected2.add(new RationalNumber(4, 1));
        
        ArrayList<RationalNumber> reducedCopy = RationalNumberListManager.copyAndReduceAll(testList);  
        assertEquals(reducedCopy.size(), testList.size());
        assertEquals(expected1.size(), testList.size());
        assertEquals(expected2.size(), testList.size());

        assertNotEquals(reducedCopy, testList);
        for (int i = 0; i < testList.size(); i++) {
            assertTrue(expected1.get(i).equalFields(testList.get(i)));
        }
        for (int i = 0; i < testList.size(); i++) {
            assertTrue(expected2.get(i).equalFields(reducedCopy.get(i)));
        }
    }
    
    @Test
    public void testSort() {
        ArrayList<RationalNumber> testList = new ArrayList<>();
        testList.add(new RationalNumber(2, 8));
        testList.add(new RationalNumber(3, 2));
        testList.add(new RationalNumber(5, 8139));
        testList.add(new RationalNumber(16, 4));
        ArrayList<RationalNumber> expected1 = new ArrayList<>();
        expected1.add(new RationalNumber(5, 8139));
        expected1.add(new RationalNumber(2, 8));
        expected1.add(new RationalNumber(3, 2));
        expected1.add(new RationalNumber(16, 4));
        ArrayList<RationalNumber> expected2 = new ArrayList<>();
        expected2.add(new RationalNumber(16, 4));
        expected2.add(new RationalNumber(3, 2));
        expected2.add(new RationalNumber(2, 8));
        expected2.add(new RationalNumber(5, 8139));
        
        
        RationalNumberListManager.sort(testList, false, false);
        for (int i = 0; i < testList.size(); i++) {
            assertTrue(expected1.get(i).equalFields(testList.get(i)));
        }
        
        RationalNumberListManager.sort(testList, false, true);
        for (int i = 0; i < testList.size(); i++) {
            assertTrue(expected2.get(i).equalFields(testList.get(i)));
        }
        testList = new ArrayList<>();
        testList.add(new RationalNumber(2, 8));
        testList.add(new RationalNumber(3, 2));
        testList.add(new RationalNumber(5, 8139));
        testList.add(new RationalNumber(16, 4));
        
        RationalNumberListManager.sort(testList, true, false);
        for (int i = 0; i < testList.size(); i++) {
            assertTrue(expected1.get(i).equalFields(testList.get(i)));
        }
        
        RationalNumberListManager.sort(testList, true, true);
        for (int i = 0; i < testList.size(); i++) {
            assertTrue(expected2.get(i).equalFields(testList.get(i)));
        }
    }
}



























