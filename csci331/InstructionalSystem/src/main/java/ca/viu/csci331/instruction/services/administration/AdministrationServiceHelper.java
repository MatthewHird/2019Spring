package ca.viu.csci331.instruction.services.administration;

import java.util.ArrayList;

import ca.viu.csci331.instruction.model.InstructorEmployment;

public class AdministrationServiceHelper {
    public static ArrayList<InstructorEmployment> employmentTypeFilter(ArrayList<InstructorEmployment> unfilteredInstructorEmploymentList, String employmentType) {
        if (employmentType.equals("both")) {
            return unfilteredInstructorEmploymentList;
        } else if (employmentType.equals("employed")) {
            return getEmployed(unfilteredInstructorEmploymentList);
        } else if (employmentType.equals("terminated")) {
            return getTerminated(unfilteredInstructorEmploymentList);
        }
        return new ArrayList<InstructorEmployment>();
    }   
    
    
    public static ArrayList<InstructorEmployment> getEmployed(ArrayList<InstructorEmployment> inList) {
        ArrayList<InstructorEmployment> outList = new ArrayList<InstructorEmployment>();
        
        for (int i = 0; i < inList.size(); i++) {
            if (inList.get(i).getEmploymentStatus().contentEquals("employed")) {
                outList.add(inList.get(i));
            }
        }
        return outList;
    }
    
    
    public static ArrayList<InstructorEmployment> getTerminated(ArrayList<InstructorEmployment> inList) {
        ArrayList<InstructorEmployment> outList = new ArrayList<InstructorEmployment>();
        
        for (int i = 0; i < inList.size(); i++) {
            if (inList.get(i).getEmploymentStatus().contentEquals("terminated")) {
                outList.add(inList.get(i));
            }
        }
        return outList;
    }
    
    
    public static String generateInstructorId() {
        int instructorId = 0;
        
        do {
            instructorId = (int) (Math.random() * 999999);
        } while (instructorId < 100000);
        
        return Integer.toString(instructorId);
    }
    
    
    public static String generateScheduleId() {
        int scheduleId = 0;
        
        do {
            scheduleId = (int) (Math.random() * 999999999);
        } while (scheduleId < 100000000);
        
        return Integer.toString(scheduleId);
    }
}
