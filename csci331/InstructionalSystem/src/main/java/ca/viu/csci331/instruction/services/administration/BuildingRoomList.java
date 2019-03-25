package ca.viu.csci331.instruction.services.administration;

import java.util.ArrayList;
import java.util.List;

import ca.viu.csci331.instruction.exception.BuildingRoomNotFoundException;
import ca.viu.csci331.instruction.exception.DuplicateBuildingRoomException;
import ca.viu.csci331.instruction.model.BuildingRoom;

/**
 * Contains and manages BuildingRoom objects in a list.
 * 
 * @author Matthew Hird
 * @date Mar. 25, 2019
 */
public class BuildingRoomList {
    private List<BuildingRoom> roomList;
    
    public BuildingRoomList() {
        roomList = new ArrayList<BuildingRoom>();
    }
    
    /**
     * @return  Number of rooms currently in BuildingRoomList.
     */
    public int getRoomCount() {
        return roomList.size();
    }
    
    /**
     * @param buildingNumber    ID of building.
     * @param roomNumber        ID of room.
     * @param capacity          Seat capacity of room.
     * @throws DuplicateBuildingRoomException   If building/room pair is already
     *                                          in BuildingRoomList.
     */
    public void add(String buildingNumber, String roomNumber, int capacity) throws DuplicateBuildingRoomException {
        if (containsBuildingRoomNumber(buildingNumber, roomNumber)) {
            throw new DuplicateBuildingRoomException(buildingNumber, roomNumber);
        }
        roomList.add(new BuildingRoom(buildingNumber, roomNumber, capacity));
    }
    
    /**
     * @param buildingNumber    ID of building.
     * @param roomNumber        ID of room.
     * @return  BuildingRoom that was removed.
     * @throws BuildingRoomNotFoundException    If BuildingRoom with building ID
     *              buildingNumber and room ID roomNumber are not in list.
     */
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
    
    /**
     * Find BuildingRoom with specified building ID and room ID.
     * 
     * @param buildingNumber    ID of building.
     * @param roomNumber        ID of room.
     * @return  BuildingRoom with building ID buildingRoom and room ID 
     *          roomNumber.
     * @throws BuildingRoomNotFoundException    If BuildingRoom with building ID
     *              buildingNumber and room ID roomNumber are not in list.
     */
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
    
    /**
     * Find BuildingRooms within a specified building.
     * 
     * @param buildingNumber    Building ID.
     * @return  List of BuildingRooms with building ID of buildingNumber.
     */
    public ArrayList<BuildingRoom> searchByBuilding(String buildingNumber) {
        ArrayList<BuildingRoom> buildingRooms = new ArrayList<BuildingRoom>();
        for (int i = 0; i < roomList.size(); i++) {
            if (roomList.get(i).getBuildingNumber().equals(buildingNumber)) {
                buildingRooms.add(roomList.get(i));
            }
        }
        return buildingRooms;
    }
    
    /**
     * Find all BuildingRooms with seat capacity greater than or equal to 
     * a specified value.
     * 
     * @param minCapacity   Minimum number a seats.
     * @return  List of BuildingRooms with capacity >= minCapacity.
     */
    public ArrayList<BuildingRoom> searchByMinCapacity(int minCapacity) {
        ArrayList<BuildingRoom> roomsWithCap = new ArrayList<BuildingRoom>();
        for (int i = 0; i < roomList.size(); i++) {
            if (roomList.get(i).getCapacity() >= minCapacity) {
                roomsWithCap.add(roomList.get(i));
            }
        }
        return roomsWithCap;
    }
    
    /**
     * @return  List of all BuildingRooms in BuildingRoomList.
     */
    public ArrayList<BuildingRoom> getAllBuildingRooms() {
        ArrayList<BuildingRoom> roomsWithCap = new ArrayList<BuildingRoom>();
        for (int i = 0; i < roomList.size(); i++) {
            roomsWithCap.add(roomList.get(i));
        }
        return roomsWithCap;
    }
    
    /**
     * Checks for existence of BuildingRoom with specified building and room.
     * 
     * @param buildingNumber    ID of building.
     * @param roomNumber        ID of room.
     * @return  true if BuildingRoom with building ID buildingNumber and room ID
     *          roomNumber exists in BuildingRoomList.
     */
    public boolean containsBuildingRoomNumber(String buildingNumber, String roomNumber) {
        for (BuildingRoom buildingRoom : roomList) {
            if (buildingRoom.getBuildingNumber().equals(buildingNumber)
                    && buildingRoom.getRoomNumber().equals(roomNumber)) {
                return true;
            }
        }
        return false;
    }
}
