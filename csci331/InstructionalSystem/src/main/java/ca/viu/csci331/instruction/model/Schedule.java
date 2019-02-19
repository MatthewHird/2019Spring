package ca.viu.csci331.instruction.model;

/**
 * A data class that represents a weekly time block for a specific room an a particular day of the week.
 * @author Matthew Hird
 * @date Feb. 12, 2019
 */
public class Schedule {
    private String day;
    private int hour;
    private int minute;
    private int duration;
    private String location;
    
    public Schedule(String day, int hour, int minute, int duration, String location) {
        this.day = day;
        this.hour = hour;
        this.minute = minute;
        this.duration = duration;
        this.location = location;
    }

    public String getDay() {
        return day;
    }

    public void setDay(String day) {
        this.day = day;
    }

    public int getHour() {
        return hour;
    }

    public void setHour(int hour) {
        this.hour = hour;
    }

    public int getMinute() {
        return minute;
    }

    public void setMinute(int minute) {
        this.minute = minute;
    }

    public int getDuration() {
        return duration;
    }

    public void setDuration(int duration) {
        this.duration = duration;
    }

    public String getLocation() {
        return location;
    }

    public void setLocation(String location) {
        this.location = location;
    }
    
    public void show() {
        System.out.printf("Day of the Week: %s \nStart Time: %02d:%02d\nDuration: %d minutes\nLocation: %s\n\n",
                day, hour, minute, duration, location);
    }
    
    public boolean equals(Schedule other) {
        if (this.getLocation().equals(other.getLocation()) && this.getDay().equals(other.getDay()) && this.getHour() == other.getHour() 
                && this.getMinute() == other.getMinute() && this.getDuration() == other.getDuration()) {
            return true;
        }
        return false;
    }
    
    public boolean doesOverlap(Schedule other) {
        if (this.getLocation().equals(other.getLocation()) && this.getDay().equals(other.getDay())) {
            int thisStart = this.getHour() * 60 + this.getMinute();
            int otherStart = other.getHour() * 60 + other.getMinute();
            
            if (thisStart <= otherStart && thisStart + this.getDuration() > otherStart) {
                return true;
            }
            if (otherStart <= thisStart && otherStart + other.getDuration() > thisStart) {
                return true;
            }
        }
        return false;
    }
}
