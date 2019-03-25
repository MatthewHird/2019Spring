package ca.viu.csci331.instruction.services.administration;

import java.util.ArrayList;

import ca.viu.csci331.instruction.model.InstructorEmployment;

/**
 * Helper class for AdministrationService class.
 * 
 * @author Matthew Hird
 * @date Mar. 25, 2019
 */
public class AdministrationServiceHelper {
    /**
     * Filters list of InstructorEmployments based on specified employment 
     * status.
     * 
     * @param unfilteredInstructorEmploymentList  List of InstructorEmployments
     *                                            to be filtered.
     * @param employmentType    Employment status to filter by.
     * @return  List of InstructorEmployments with the employment status of
     *          employmentType.
     */
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
    
    /**
     * Filters list of InstructorEmployments where 
     * employment status = "employed".
     * 
     * @param inList  List of InstructorEmployments to be filtered.
     * @return  List of InstructorEmployments with the employment status of
     *          "employed".
     */
    public static ArrayList<InstructorEmployment> getEmployed(ArrayList<InstructorEmployment> inList) {
        ArrayList<InstructorEmployment> outList = new ArrayList<InstructorEmployment>();
        
        for (int i = 0; i < inList.size(); i++) {
            if (inList.get(i).getEmploymentStatus().contentEquals("employed")) {
                outList.add(inList.get(i));
            }
        }
        return outList;
    }
    
    /**
     * Filters list of InstructorEmployments where 
     * employment status = "terminated".
     * 
     * @param inList  List of InstructorEmployments to be filtered.
     * @return  List of InstructorEmployments with the employment status of
     *          "terminated".
     */
    public static ArrayList<InstructorEmployment> getTerminated(ArrayList<InstructorEmployment> inList) {
        ArrayList<InstructorEmployment> outList = new ArrayList<InstructorEmployment>();
        
        for (int i = 0; i < inList.size(); i++) {
            if (inList.get(i).getEmploymentStatus().contentEquals("terminated")) {
                outList.add(inList.get(i));
            }
        }
        return outList;
    }
    
    /**
     * @return  String in the range of "100000" to "999999".
     */
    public static String generateInstructorId() {
        int instructorId = 0;
        
        do {
            instructorId = (int) (Math.random() * 999999);
        } while (instructorId < 100000);
        
        return Integer.toString(instructorId);
    }
    
    /**
     * @return  String in the range of "100000000" to "999999999".
     */
    public static String generateScheduleId() {
        int scheduleId = 0;
        
        do {
            scheduleId = (int) (Math.random() * 999999999);
        } while (scheduleId < 100000000);
        
        return Integer.toString(scheduleId);
    }
}
