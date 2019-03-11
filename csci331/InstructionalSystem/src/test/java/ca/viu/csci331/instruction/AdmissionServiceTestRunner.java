package ca.viu.csci331.instruction;

import org.junit.runner.JUnitCore;

public class AdmissionServiceTestRunner {

    public static void main(String[] args) {
        System.out.print("Running Admission Service unit tests of Instructional System:\n\n");
        JUnitCore.main("ca.viu.csci331.instruction.services.admission.StudentAdmissionListTests",
                "ca.viu.csci331.instruction.services.admission.AdmissionServiceHelperTests",
                "ca.viu.csci331.instruction.services.admission.AdmissionServiceTests",
                "ca.viu.csci331.instruction.retrieval.admission.AdmissionRetrievalTests",
                "ca.viu.csci331.instruction.persistence.admission.AdmissionPersistenceTests");
        
    }

}
