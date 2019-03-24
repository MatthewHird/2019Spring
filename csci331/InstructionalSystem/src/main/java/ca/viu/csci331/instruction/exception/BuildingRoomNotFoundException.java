package ca.viu.csci331.instruction.exception;

import ca.viu.csci331.instruction.model.BuildingRoom;

public class BuildingRoomNotFoundException extends Exception {

    private static final long serialVersionUID = 1L;

    public BuildingRoomNotFoundException(String buildingNumber, String roomNumber) {
        super("Building: " + buildingNumber + " Room: " + roomNumber + " not found");
    }
    
    public BuildingRoomNotFoundException(BuildingRoom buildingRoom) {
        super("Building: " + buildingRoom.getBuildingNumber() + " Room: " 
                + buildingRoom.getRoomNumber() + " not found");
    }
}
