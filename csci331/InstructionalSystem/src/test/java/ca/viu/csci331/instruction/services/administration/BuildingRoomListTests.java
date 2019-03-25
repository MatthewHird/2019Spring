package ca.viu.csci331.instruction.services.administration;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collections;

import org.junit.Before;
import org.junit.Test;

import ca.viu.csci331.instruction.exception.BuildingRoomNotFoundException;
import ca.viu.csci331.instruction.exception.DuplicateBuildingRoomException;
import ca.viu.csci331.instruction.model.BuildingRoom;

public class BuildingRoomListTests {
    private BuildingRoomList buildingRoomList;
    
    @Before
    public void populateBuildingRoomList() {
        buildingRoomList = new BuildingRoomList();
        
        try {
            buildingRoomList.add("B001", "R001", 10);
            buildingRoomList.add("B001", "R002", 100);
            buildingRoomList.add("B002", "R001", 1);
            buildingRoomList.add("B002", "R002", 11);
        } catch (DuplicateBuildingRoomException e) {
            e.printStackTrace();
        }
    }
    
    @Test
    public void testAdd() throws DuplicateBuildingRoomException {
        ArrayList<BuildingRoom> expected = new ArrayList<BuildingRoom>();
        expected.add(new BuildingRoom("B001", "R001", 10));
        expected.add(new BuildingRoom("B001", "R002", 100));
        expected.add(new BuildingRoom("B002", "R001", 1));
        expected.add(new BuildingRoom("B002", "R002", 11));
        expected.add(new BuildingRoom("B003", "R003", 11));

        buildingRoomList.add("B003", "R003", 11);
        assertTrue(compareBuildingRoomArrayList(expected, buildingRoomList.getAllBuildingRooms()));
    }
    
    @Test
    public void testRemove() throws BuildingRoomNotFoundException {
        ArrayList<BuildingRoom> expected = new ArrayList<BuildingRoom>();
        expected.add(new BuildingRoom("B001", "R001", 10));
        expected.add(new BuildingRoom("B001", "R002", 100));
        expected.add(new BuildingRoom("B002", "R001", 1));
        
        buildingRoomList.remove("B002", "R002");
        assertTrue(compareBuildingRoomArrayList(expected, buildingRoomList.getAllBuildingRooms()));
    }
    
    @Test
    public void testSearchByBuildNumRoomNum() throws BuildingRoomNotFoundException {
        BuildingRoom expected = new BuildingRoom("B001", "R002", 100);
        assertTrue(expected.equalBuildNumRoomNum(buildingRoomList.searchByBuildNumRoomNum("B001", "R002")));
    }
    
    @Test
    public void testSearchByBuilding() {
        ArrayList<BuildingRoom> expected = new ArrayList<BuildingRoom>();
        expected.add(new BuildingRoom("B002", "R002", 11));
        expected.add(new BuildingRoom("B001", "R002", 100));
        
        assertTrue(compareBuildingRoomArrayList(expected, buildingRoomList.searchByMinCapacity(11)));
    }
    
    @Test
    public void testSearchByMinCapacity() {
        ArrayList<BuildingRoom> expected = new ArrayList<BuildingRoom>();
        expected.add(new BuildingRoom("B002", "R001", 1));
        expected.add(new BuildingRoom("B002", "R002", 11));
        
        assertTrue(compareBuildingRoomArrayList(expected, buildingRoomList.searchByBuilding("B002")));
    }
    
    @Test
    public void testContainsBuildingRoomNumber() {
        assertTrue(buildingRoomList.containsBuildingRoomNumber("B001", "R001"));
        assertFalse(buildingRoomList.containsBuildingRoomNumber("B004", "B004"));
    }
    
    private boolean compareBuildingRoomArrayList(ArrayList<BuildingRoom> list1, ArrayList<BuildingRoom> list2) { 
        if (list1.size() == list2.size()) {
            Collections.sort(list1);
            Collections.sort(list2);
            for (int i = 0; i < list1.size(); i++) {
                if (!list1.get(i).equalBuildNumRoomNum(list2.get(i))) {
                    return false;
                }
            }
        } else {
            return false;
        }
        return true;
    }
}
