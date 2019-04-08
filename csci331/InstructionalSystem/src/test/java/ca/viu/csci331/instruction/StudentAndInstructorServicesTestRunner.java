package ca.viu.csci331.instruction;

import org.junit.runner.JUnitCore;

public class StudentAndInstructorServicesTestRunner {

    public static void main(String[] args) {
        System.out.print("Running Student Service and Instructor Service unit tests of Instructional System:\n\n");
        JUnitCore.main("ca.viu.csci331.instruction.services.student.EnrollmentListTests",
                "ca.viu.csci331.instruction.services.student.StudentServiceTests",
                "ca.viu.csci331.instruction.services.instructor.InstructorServiceTests");
    }

}
