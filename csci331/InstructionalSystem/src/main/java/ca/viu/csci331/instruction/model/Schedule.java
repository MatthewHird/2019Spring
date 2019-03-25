package ca.viu.csci331.instruction.model;

import java.time.LocalTime;
import java.util.regex.Pattern;

import ca.viu.csci331.instruction.exception.InvalidDayOfWeekRuntimeException;
import ca.viu.csci331.instruction.exception.InvalidDurationRuntimeException;

/**
 * A data class that represents a weekly time block for a specific room an a particular day of the week.
 * @author Matthew Hird
 * @date Mar 23, 2019
 */
public class Schedule implements Comparable<Object> {
    private String scheduleId;
    private String day;
    private LocalTime startTime;
    private int duration;
    private BuildingRoom location;
    
    static class DayOfWeek {
        static public String of(String inputDay) {
            String d = inputDay.toLowerCase();
            String dayOfWeek = "";
            if (Pattern.matches("^sun(day)?$", d)) {
                dayOfWeek = "Sunday";
            } else if (Pattern.matches("^mon(day)?$", d)) {
                dayOfWeek = "Monday";
            } else if (Pattern.matches("^tue(s)?(day)?$", d)) {
                dayOfWeek = "Tuesday";
            } else if (Pattern.matches("^wed(nesday)?$", d)) {
                dayOfWeek = "Wednesday";
            } else if (Pattern.matches("^thu(r(s)?)?(day)?$", d)) {
                dayOfWeek = "Thursday";
            } else if (Pattern.matches("^fri(day)?$", d)) {
                dayOfWeek = "Friday";
            } else if (Pattern.matches("^sat(urday)?$", d)) {
                dayOfWeek = "Saturday";
            } else {
                throw new InvalidDayOfWeekRuntimeException(inputDay);
            }
            return dayOfWeek;
        }
    }
    
    public Schedule(String scheduleId, String dayOfWeek, LocalTime startTime, int duration, BuildingRoom location) {
        if (duration <= 0) {
            throw new InvalidDurationRuntimeException(duration);
        }
        this.scheduleId = scheduleId;
        this.day = DayOfWeek.of(dayOfWeek);
        this.startTime = startTime;
        this.duration = duration;
        this.location = location;
    }
    
    public String getScheduleId() {
        return scheduleId;
    }
    
    public void setScheduleId(String scheduleId) {
        this.scheduleId = scheduleId;
    }

    public String getDay() {
        return day;
    }

    public void setDay(String day) {
        this.day = DayOfWeek.of(day);
    }

    public LocalTime getStartTime() {
        return startTime;
    }

    public void setStartTime(LocalTime startTime) {
        this.startTime = startTime;
    }

    public int getDuration() {
        return duration;
    }

    public void setDuration(int duration) {
        if (duration <= 0) {
            throw new InvalidDurationRuntimeException(duration);
        }
        this.duration = duration;
    }

    public BuildingRoom getLocation() {
        return location;
    }

    public void setLocation(BuildingRoom location) {
        this.location = location;
    }
    
    public void show() {
        System.out.printf("Day of the Week: %s \nStart Time: %s\nDuration: %d minutes\n"
                + "Location:\n        Building: %s\n        Room: %s\n        Capacity: %d\n\n",
                getDay(), getStartTime().toString(), getDuration(), getLocation().getBuildingNumber(),
                getLocation().getRoomNumber(), getLocation().getCapacity());
    }
    
    
    public boolean doesOverlap(Schedule other) {
        if (this.getLocation().getBuildingNumber().equals(other.getLocation().getBuildingNumber()) 
                && this.getLocation().getRoomNumber().equals(other.getLocation().getRoomNumber())
                && this.getDay().equals(other.getDay())) {
            LocalTime thisStart = this.getStartTime();
            LocalTime otherStart = other.getStartTime();
            
            if (!thisStart.isAfter(otherStart) 
                    && thisStart.plusMinutes(this.getDuration()).isAfter(otherStart)) {
                return true;
            }
            if (!otherStart.isAfter(thisStart) 
                    && otherStart.plusMinutes(other.getDuration()).isAfter(thisStart)) {
                return true;
            }
        }
        return false;
    }
    
    public boolean dayEquals(String dayOfWeek) {
        if (this.getDay().equals(DayOfWeek.of(dayOfWeek))) {
            return true;
        }
        return false;
    }
    
    public boolean scheduleIdEquals(Schedule other) {
        return this.getScheduleId().equals(other.getScheduleId());
    }
    
    public boolean scheduleIdEquals(String otherScheduleId) {
        return this.getScheduleId().equals(otherScheduleId);
    }
    
    @Override
    public String toString() {
        return String.format( "%s\n%s\n%d\n%s", getDay(), getStartTime().toString(), 
                getDuration(), getLocation().toString());
    }
    
    @Override
    public int compareTo(Object o) {
        Schedule s = (Schedule) o;
        int result = this.getDay().compareToIgnoreCase(s.getDay());
        if (result == 0) {
            result = this.getStartTime().compareTo(s.getStartTime());
            if (result == 0) {
                result = this.getDuration() - s.getDuration();
                if (result == 0) {
                    result = this.getLocation().compareTo(s.getLocation());
                }
            }
        }
        return result;
    }
}
