package ca.viu.csci331.instruction.model;

import ca.viu.csci331.instruction.exception.InvalidRoomCapacityRuntimeException;

public class BuildingRoom implements Comparable<Object> {
    private String buildingNumber;
    private String roomNumber;
    private int capacity;
    
    public BuildingRoom(String buildingNumber, String roomNumber, int capacity) {
        if (capacity < 0) {
            throw new InvalidRoomCapacityRuntimeException(capacity);
        }
        this.capacity = capacity;
        this.buildingNumber = buildingNumber;
        this.roomNumber = roomNumber;
        this.capacity = capacity;
    }

    public String getBuildingNumber() {
        return buildingNumber;
    }

    public void setBuildingNumber(String buildingNumber) {
        this.buildingNumber = buildingNumber;
    }

    public String getRoomNumber() {
        return roomNumber;
    }

    public void setRoomNumber(String roomNumber) {
        this.roomNumber = roomNumber;
    }

    public int getCapacity() {
        return capacity;
    }

    public void setCapacity(int capacity) {
        if (capacity < 0) {
            throw new InvalidRoomCapacityRuntimeException(capacity);
        }
        this.capacity = capacity;
    }
    
    public boolean equalBuildNumRoomNum(BuildingRoom other) {
        if (this.getBuildingNumber().toLowerCase().equals(other.getBuildingNumber().toLowerCase())
                && this.getRoomNumber().toLowerCase().equals(other.getRoomNumber().toLowerCase())) {
            return true;
        }
        return false;
    }
    
    public boolean equalBuildNumRoomNum(String buildingNumber, String roomNumber) {
        if (this.getBuildingNumber().toLowerCase().equals(buildingNumber.toLowerCase())
                && this.getRoomNumber().toLowerCase().equals(roomNumber.toLowerCase())) {
            return true;
        }
        return false;
    }
    
    public void show() {
        System.out.printf("Building Number: %s\nRoom Number: %s\nRoom Capacity: %d\n\n", 
                getBuildingNumber(), getRoomNumber(), getCapacity());
    }
    
    @Override
    public String toString() {
        return String.format("%s\n%s\n%d\n", getBuildingNumber(), getRoomNumber(), getCapacity());
    }
    
    @Override
    public int compareTo(Object o) {
        BuildingRoom s = (BuildingRoom) o;
        int result = this.getBuildingNumber().compareToIgnoreCase(s.getBuildingNumber());
        if (result == 0) {
            result = this.getRoomNumber().compareToIgnoreCase(s.getRoomNumber());
        }
        return result;
    }
}
