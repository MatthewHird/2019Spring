package ca.viu.csci331.instruction.services.administration;

import java.util.ArrayList;
import java.util.List;

import ca.viu.csci331.instruction.exception.BuildingRoomNotFoundException;
import ca.viu.csci331.instruction.exception.DuplicateBuildingRoomException;
import ca.viu.csci331.instruction.model.BuildingRoom;

public class BuildingRoomList {
    private List<BuildingRoom> roomList;
    
    public BuildingRoomList() {
        roomList = new ArrayList<BuildingRoom>();
    }
    
    public int getRoomCount() {
        return roomList.size();
    }
    
    public void add(BuildingRoom addedRoom) throws DuplicateBuildingRoomException {
        if (containsBuildingRoom(addedRoom)) {
            throw new DuplicateBuildingRoomException(addedRoom);
        }
        roomList.add(addedRoom);
    }
    
    public void add(String buildingNumber, String roomNumber, int capacity) throws DuplicateBuildingRoomException {
        if (containsBuildingRoomNumber(buildingNumber, roomNumber)) {
            throw new DuplicateBuildingRoomException(buildingNumber, roomNumber);
        }
        roomList.add(new BuildingRoom(buildingNumber, roomNumber, capacity));
    }
    
    public BuildingRoom remove(BuildingRoom addedRoom) throws BuildingRoomNotFoundException {
        int removeIndex = -1;
        for (int i = 0; i < roomList.size(); i++) {
            if (roomList.get(i).getBuildingNumber().equals(addedRoom.getBuildingNumber())
                    && roomList.get(i).getRoomNumber().equals(addedRoom.getRoomNumber())) {
                    removeIndex = i;
                    break;
            }
        }
        if (removeIndex == -1) {
            throw new BuildingRoomNotFoundException(addedRoom);
        }
        return roomList.remove(removeIndex);
    }
    
    public BuildingRoom remove(String buildingNumber, String roomNumber) throws BuildingRoomNotFoundException {
        int removeIndex = -1;
        for (int i = 0; i < roomList.size(); i++) {
            if (roomList.get(i).getBuildingNumber().equals(buildingNumber)
                    && roomList.get(i).getRoomNumber().equals(roomNumber)) {
                removeIndex = i;
                break;
            }
        }
        if (removeIndex == -1) {
            throw new BuildingRoomNotFoundException(buildingNumber, roomNumber);
        }
        return roomList.remove(removeIndex);
    }
    
    public BuildingRoom searchByBuildNumRoomNum(String buildingNumber, String roomNumber) 
            throws BuildingRoomNotFoundException {
        for (int i = 0; i < roomList.size(); i++) {
            if (roomList.get(i).getBuildingNumber().equals(buildingNumber)
                    && roomList.get(i).getRoomNumber().equals(roomNumber)) {
                return roomList.get(i);
            }
        }
        throw new BuildingRoomNotFoundException(buildingNumber, roomNumber);
    }
    
    public ArrayList<BuildingRoom> searchByBuilding(String buildingNumber) {
        ArrayList<BuildingRoom> buildingRooms = new ArrayList<BuildingRoom>();
        for (int i = 0; i < roomList.size(); i++) {
            if (roomList.get(i).getBuildingNumber().equals(buildingNumber)) {
                buildingRooms.add(roomList.get(i));
            }
        }
        return buildingRooms;
    }
    
    public ArrayList<BuildingRoom> searchByMinCapacity(int minCapacity) {
        ArrayList<BuildingRoom> roomsWithCap = new ArrayList<BuildingRoom>();
        for (int i = 0; i < roomList.size(); i++) {
            if (roomList.get(i).getCapacity() >= minCapacity) {
                roomsWithCap.add(roomList.get(i));
            }
        }
        return roomsWithCap;
    }
    
    public boolean containsBuildingRoomNumber(String buildingNumber, String roomNumber) {
        for (BuildingRoom buildingRoom : roomList) {
            if (buildingRoom.getBuildingNumber().equals(buildingNumber)
                    && buildingRoom.getRoomNumber().equals(roomNumber)) {
                return true;
            }
        }
        return false;
    }
    
    public boolean containsBuildingRoom(BuildingRoom buildingRoom) {
        return containsBuildingRoomNumber(buildingRoom.getBuildingNumber(), 
                buildingRoom.getRoomNumber());
    }
}
