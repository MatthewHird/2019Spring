package ca.viu.csci331.instruction;

import org.junit.runner.JUnitCore;

public class AdministrationServiceTestRunner {

    public static void main(String[] args) {
        System.out.print("Running Administration Service unit tests of Instructional System:\n\n");
        JUnitCore.main(
                "ca.viu.csci331.instruction.services.administration.AdministrationServiceHelperTests",
                "ca.viu.csci331.instruction.services.administration.AdministrationServiceTests",
                "ca.viu.csci331.instruction.services.administration.BuildingRoomListTests",
                "ca.viu.csci331.instruction.services.administration.InstructorEmploymentListTests",
                "ca.viu.csci331.instruction.services.administration.OfferedCourseListTests",
                "ca.viu.csci331.instruction.services.administration.ScheduleListTests",
                "ca.viu.csci331.instruction.services.administration.SeminarListTests");
    }
}
