package ca.viu.csci331.instruction.exception;

import ca.viu.csci331.instruction.model.BuildingRoom;

public class DuplicateBuildingRoomException extends Exception {
    private static final long serialVersionUID = 1L;
    
    public DuplicateBuildingRoomException(BuildingRoom buildingRoom) {
        super("Building: " + buildingRoom.getBuildingNumber() 
                + " Room: " + buildingRoom.getRoomNumber() + " already exists");
    }
    
    public DuplicateBuildingRoomException(String buildingNumber, String roomNumber) {
        super("Building: " + buildingNumber + " Room: " + roomNumber + " already exists");
    }
}
