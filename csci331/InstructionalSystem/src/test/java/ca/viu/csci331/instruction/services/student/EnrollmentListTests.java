package ca.viu.csci331.instruction.services.student;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Collections;

import org.junit.Before;
import org.junit.Test;

import ca.viu.csci331.instruction.exception.DuplicateScheduleException;
import ca.viu.csci331.instruction.exception.EnrollmentNotFoundException;
import ca.viu.csci331.instruction.model.BuildingRoom;
import ca.viu.csci331.instruction.model.Course;
import ca.viu.csci331.instruction.model.Enrollment;
import ca.viu.csci331.instruction.model.Instructor;
import ca.viu.csci331.instruction.model.Schedule;
import ca.viu.csci331.instruction.model.Seminar;
import ca.viu.csci331.instruction.model.Student;

public class EnrollmentListTests {
    private EnrollmentList enrollmentList;
    
    @Before
    public void prepareEnrollmentList() {
        enrollmentList = new EnrollmentList();
        Instructor ins1 = new Instructor("Instructor1", "ins1", "i1@mail.com");
        Instructor ins2 = new Instructor("Instructor2", "ins2", "i2@mail.com");
        Student stu1 = new Student("Student1", "stu1", "s1@mail.com");
        Student stu2 = new Student("Student2", "stu2", "s2@mail.com");
        Student stu3 = new Student("Student3", "stu3", "s3@mail.com");
        Student stu4 = new Student("Student4", "stu4", "s4@mail.com");
        Course cou1 = new Course("Course 1", "cou1", 3, "Desc1");
        Course cou2 = new Course("Course 2", "cou2", 3, "Desc2");
        Course cou3 = new Course("Course 3", "cou3", 3, "Desc3");
        Schedule she1 = new Schedule("she1", "Monday", LocalTime.of(9, 0), 60, new BuildingRoom("B001", "R001", 50));
        Schedule she2 = new Schedule("she2", "Tuesday", LocalTime.of(9, 0), 60, new BuildingRoom("B001", "R001", 50));
        Schedule she3 = new Schedule("she3", "Wednesday", LocalTime.of(9, 0), 60, new BuildingRoom("B001", "R001", 50));
        Schedule she4 = new Schedule("she4", "Monday", LocalTime.of(11, 30), 120, new BuildingRoom("B002", "R002", 50));
        Schedule she5 = new Schedule("she5", "Friday", LocalTime.of(14, 0), 90, new BuildingRoom("B001", "R001", 50));
        Schedule she6 = new Schedule("she6", "Thursday", LocalTime.of(10, 0), 60, new BuildingRoom("B002", "R002", 50));
        Seminar sem1 = new Seminar("sem1", cou1, 20, ins1);
        Seminar sem2 = new Seminar("sem2", cou2, 20, ins2);
        Seminar sem3 = new Seminar("sem3", cou2, 20, ins1);
        Seminar sem4 = new Seminar("sem4", cou3, 20, ins2);
        
        try {
            sem1.addSchedule(she1);
            sem1.addSchedule(she2);
            sem2.addSchedule(she3);
            sem3.addSchedule(she4);
            sem4.addSchedule(she5);
            sem4.addSchedule(she6);
        } catch (DuplicateScheduleException e) {
            e.printStackTrace();
        }
        
        Enrollment enr1 = new Enrollment(stu1, sem1);
        Enrollment enr2 = new Enrollment(stu1, sem2);
        enr2.setGrade(82.1);
        enr2.setComplete(true);
        Enrollment enr3 = new Enrollment(stu1, sem4);
        enr3.setGrade(75.0);
        enr3.setComplete(true);
        Enrollment enr4 = new Enrollment(stu2, sem1);
        Enrollment enr5 = new Enrollment(stu2, sem3);
        Enrollment enr6 = new Enrollment(stu3, sem1);
        Enrollment enr7 = new Enrollment(stu3, sem4);
        enr7.setGrade(94.3);
        enr7.setComplete(true);
        Enrollment enr8 = new Enrollment(stu4, sem2);
        enr8.setGrade(55.0);
        enr8.setComplete(true);
        
        enrollmentList.add(enr1);
        enrollmentList.add(enr2);
        enrollmentList.add(enr3);
        enrollmentList.add(enr4);
        enrollmentList.add(enr5);
        enrollmentList.add(enr6);
        enrollmentList.add(enr7);
        enrollmentList.add(enr8);
    }
    
    @Test
    public void testAdd() {
        Student stu1 = new Student("Student1", "stu1", "s1@mail.com");
        Student stu2 = new Student("Student2", "stu2", "s2@mail.com");
        Student stu3 = new Student("Student3", "stu3", "s3@mail.com");
        Student stu4 = new Student("Student4", "stu4", "s4@mail.com");
        Seminar sem1 = new Seminar("sem1", new Course("", "", 0, ""), 20, new Instructor("", "", ""));
        Seminar sem2 = new Seminar("sem2", new Course("", "", 0, ""), 20, new Instructor("", "", ""));
        Seminar sem3 = new Seminar("sem3", new Course("", "", 0, ""), 20, new Instructor("", "", ""));
        Seminar sem4 = new Seminar("sem4", new Course("", "", 0, ""), 20, new Instructor("", "", ""));
        Enrollment enr1 = new Enrollment(stu1, sem1);
        Enrollment enr2 = new Enrollment(stu1, sem2);
        enr2.setGrade(82.1);
        enr2.setComplete(true);
        Enrollment enr3 = new Enrollment(stu1, sem4);
        enr3.setGrade(75.0);
        enr3.setComplete(true);
        Enrollment enr4 = new Enrollment(stu2, sem1);
        Enrollment enr5 = new Enrollment(stu2, sem3);
        Enrollment enr6 = new Enrollment(stu3, sem1);
        Enrollment enr7 = new Enrollment(stu3, sem4);
        enr7.setGrade(94.3);
        enr7.setComplete(true);
        Enrollment enr8 = new Enrollment(stu4, sem2);
        enr8.setGrade(55.0);
        enr8.setComplete(true);
        
        ArrayList<Enrollment> expected = new ArrayList<Enrollment>();
        
        expected.add(enr1);
        expected.add(enr2);
        expected.add(enr3);
        expected.add(enr4);
        expected.add(enr5);
        expected.add(enr6);
        expected.add(enr7);
        expected.add(enr8);
        
        Enrollment enr9 = new Enrollment(stu1, sem3);
        
        expected.add(enr9);
        enrollmentList.add(enr9);
        
        assertTrue(compareEnrollmentArrayList(expected, enrollmentList.getAllEnrollments()));
    }
    
    @Test
    public void testRemove() throws EnrollmentNotFoundException {
        Student stu1 = new Student("Student1", "stu1", "s1@mail.com");
        Student stu2 = new Student("Student2", "stu2", "s2@mail.com");
        Student stu3 = new Student("Student3", "stu3", "s3@mail.com");
        Student stu4 = new Student("Student4", "stu4", "s4@mail.com");
        Seminar sem1 = new Seminar("sem1", new Course("", "", 0, ""), 20, new Instructor("", "", ""));
        Seminar sem2 = new Seminar("sem2", new Course("", "", 0, ""), 20, new Instructor("", "", ""));
        Seminar sem3 = new Seminar("sem3", new Course("", "", 0, ""), 20, new Instructor("", "", ""));
        Seminar sem4 = new Seminar("sem4", new Course("", "", 0, ""), 20, new Instructor("", "", ""));
        Enrollment enr1 = new Enrollment(stu1, sem1);
        Enrollment enr2 = new Enrollment(stu1, sem2);
        enr2.setGrade(82.1);
        enr2.setComplete(true);
        Enrollment enr3 = new Enrollment(stu1, sem4);
        enr3.setGrade(75.0);
        enr3.setComplete(true);
        Enrollment enr4 = new Enrollment(stu2, sem1);
        Enrollment enr5 = new Enrollment(stu2, sem3);
        Enrollment enr7 = new Enrollment(stu3, sem4);
        enr7.setGrade(94.3);
        enr7.setComplete(true);
        Enrollment enr8 = new Enrollment(stu4, sem2);
        enr8.setGrade(55.0);
        enr8.setComplete(true);
        
        ArrayList<Enrollment> expected = new ArrayList<Enrollment>();
        
        expected.add(enr1);
        expected.add(enr2);
        expected.add(enr3);
        expected.add(enr4);
        expected.add(enr5);
        expected.add(enr7);
        expected.add(enr8);
        
        enrollmentList.remove("stu3", "sem1");
        
        assertTrue(compareEnrollmentArrayList(expected, enrollmentList.getAllEnrollments()));
    }
    
    @Test
    public void testSearchByStuIdSemId() throws EnrollmentNotFoundException {
        Student stu1 = new Student("Student1", "stu1", "s1@mail.com");
        Seminar sem2 = new Seminar("sem2", new Course("", "", 0, ""), 20, new Instructor("", "", ""));

        Enrollment enr2 = new Enrollment(stu1, sem2);
        enr2.setGrade(82.1);
        enr2.setComplete(true);
        
        Enrollment expected = enr2;
        
        assertTrue(expected.equalStuIdSemId(enrollmentList.searchByStuIdSemId("stu1", "sem2")));
    }
    
    @Test
    public void testSearchByStudentId() {
        Student stu1 = new Student("Student1", "stu1", "s1@mail.com");
        Seminar sem1 = new Seminar("sem1", new Course("", "", 0, ""), 20, new Instructor("", "", ""));
        Seminar sem2 = new Seminar("sem2", new Course("", "", 0, ""), 20, new Instructor("", "", ""));
        Seminar sem4 = new Seminar("sem4", new Course("", "", 0, ""), 20, new Instructor("", "", ""));
        Enrollment enr1 = new Enrollment(stu1, sem1);
        Enrollment enr2 = new Enrollment(stu1, sem2);
        enr2.setGrade(82.1);
        enr2.setComplete(true);
        Enrollment enr3 = new Enrollment(stu1, sem4);
        enr3.setGrade(75.0);
        enr3.setComplete(true);
        
        ArrayList<Enrollment> expected = new ArrayList<Enrollment>();
        
        expected.add(enr1);
        expected.add(enr2);
        expected.add(enr3);
        
        assertTrue(compareEnrollmentArrayList(expected, enrollmentList.searchByStudentId(stu1.getStudentId())));
    }
    
    @Test
    public void testSearchCompletedByStudentId() {
        Student stu1 = new Student("Student1", "stu1", "s1@mail.com");
        Seminar sem2 = new Seminar("sem2", new Course("", "", 0, ""), 20, new Instructor("", "", ""));
        Seminar sem4 = new Seminar("sem4", new Course("", "", 0, ""), 20, new Instructor("", "", ""));
        Enrollment enr2 = new Enrollment(stu1, sem2);
        enr2.setGrade(82.1);
        enr2.setComplete(true);
        Enrollment enr3 = new Enrollment(stu1, sem4);
        enr3.setGrade(75.0);
        enr3.setComplete(true);
        
        ArrayList<Enrollment> expected = new ArrayList<Enrollment>();
        
        expected.add(enr2);
        expected.add(enr3);
        
        assertTrue(compareEnrollmentArrayList(expected, enrollmentList.searchCompletedByStudentId(stu1.getStudentId())));
    }
    
    @Test
    public void testSearchIncompleteByStudentId() {
        Student stu1 = new Student("Student1", "stu1", "s1@mail.com");
        Seminar sem1 = new Seminar("sem1", new Course("", "", 0, ""), 20, new Instructor("", "", ""));

        Enrollment enr1 = new Enrollment(stu1, sem1);
        
        ArrayList<Enrollment> expected = new ArrayList<Enrollment>();
        
        expected.add(enr1);
        
        assertTrue(compareEnrollmentArrayList(expected, enrollmentList.searchIncompleteByStudentId(stu1.getStudentId())));
    }
    
    @Test
    public void testSearchByInstructorId() throws DuplicateScheduleException {
        Instructor ins1 = new Instructor("Instructor1", "ins1", "i1@mail.com");
        Student stu1 = new Student("Student1", "stu1", "s1@mail.com");
        Student stu2 = new Student("Student2", "stu2", "s2@mail.com");
        Student stu3 = new Student("Student3", "stu3", "s3@mail.com");
        Course cou1 = new Course("Course 1", "cou1", 3, "Desc1");
        Course cou2 = new Course("Course 2", "cou2", 3, "Desc2");
        Schedule she1 = new Schedule("she1", "Monday", LocalTime.of(9, 0), 60, new BuildingRoom("B001", "R001", 50));
        Schedule she2 = new Schedule("she2", "Tuesday", LocalTime.of(9, 0), 60, new BuildingRoom("B001", "R001", 50));
        Schedule she4 = new Schedule("she4", "Monday", LocalTime.of(11, 30), 120, new BuildingRoom("B002", "R002", 50));
        Seminar sem1 = new Seminar("sem1", cou1, 20, ins1);
        Seminar sem3 = new Seminar("sem3", cou2, 20, ins1);
        
        sem1.addSchedule(she1);
        sem1.addSchedule(she2);
        sem3.addSchedule(she4);
        
        Enrollment enr1 = new Enrollment(stu1, sem1);
        Enrollment enr4 = new Enrollment(stu2, sem1);
        Enrollment enr5 = new Enrollment(stu2, sem3);
        Enrollment enr6 = new Enrollment(stu3, sem1);
        
        ArrayList<Enrollment> expected = new ArrayList<Enrollment>();
        expected.add(enr1);
        expected.add(enr4);
        expected.add(enr5);
        expected.add(enr6);
        
        assertTrue(compareEnrollmentArrayList(expected, enrollmentList.searchByInstructorId("ins1")));
    }
    
    @Test
    public void testSearchIncompleteByInstructorId() throws DuplicateScheduleException {
        Instructor ins1 = new Instructor("Instructor1", "ins1", "i1@mail.com");
        Student stu1 = new Student("Student1", "stu1", "s1@mail.com");
        Student stu2 = new Student("Student2", "stu2", "s2@mail.com");
        Student stu3 = new Student("Student3", "stu3", "s3@mail.com");
        Course cou1 = new Course("Course 1", "cou1", 3, "Desc1");
        Course cou2 = new Course("Course 2", "cou2", 3, "Desc2");
        Schedule she1 = new Schedule("she1", "Monday", LocalTime.of(9, 0), 60, new BuildingRoom("B001", "R001", 50));
        Schedule she2 = new Schedule("she2", "Tuesday", LocalTime.of(9, 0), 60, new BuildingRoom("B001", "R001", 50));
        Schedule she4 = new Schedule("she4", "Monday", LocalTime.of(11, 30), 120, new BuildingRoom("B002", "R002", 50));
        Seminar sem1 = new Seminar("sem1", cou1, 20, ins1);
        Seminar sem3 = new Seminar("sem3", cou2, 20, ins1);
        
        sem1.addSchedule(she1);
        sem1.addSchedule(she2);
        sem3.addSchedule(she4);
        
        Enrollment enr1 = new Enrollment(stu1, sem1);
        Enrollment enr4 = new Enrollment(stu2, sem1);
        Enrollment enr5 = new Enrollment(stu2, sem3);
        Enrollment enr6 = new Enrollment(stu3, sem1);
        
        ArrayList<Enrollment> expected = new ArrayList<Enrollment>();
        expected.add(enr1);
        expected.add(enr4);
        expected.add(enr5);
        expected.add(enr6);
        
        assertTrue(compareEnrollmentArrayList(expected, enrollmentList.searchIncompleteByInstructorId("ins1")));
    }
    
    @Test
    public void testSearchBySeminarId() throws DuplicateScheduleException {
        Instructor ins1 = new Instructor("Instructor1", "ins1", "i1@mail.com");
        Student stu1 = new Student("Student1", "stu1", "s1@mail.com");
        Student stu2 = new Student("Student2", "stu2", "s2@mail.com");
        Student stu3 = new Student("Student3", "stu3", "s3@mail.com");
        Course cou1 = new Course("Course 1", "cou1", 3, "Desc1");
        Schedule she1 = new Schedule("she1", "Monday", LocalTime.of(9, 0), 60, new BuildingRoom("B001", "R001", 50));
        Schedule she2 = new Schedule("she2", "Tuesday", LocalTime.of(9, 0), 60, new BuildingRoom("B001", "R001", 50));
        Seminar sem1 = new Seminar("sem1", cou1, 20, ins1);
        
        sem1.addSchedule(she1);
        sem1.addSchedule(she2);
        
        Enrollment enr1 = new Enrollment(stu1, sem1);
        Enrollment enr4 = new Enrollment(stu2, sem1);
        Enrollment enr6 = new Enrollment(stu3, sem1);
        
        ArrayList<Enrollment> expected = new ArrayList<Enrollment>();
        expected.add(enr1);
        expected.add(enr4);
        expected.add(enr6);
        
        assertTrue(compareEnrollmentArrayList(expected, enrollmentList.searchBySeminarId("sem1")));
    }
    
    @Test
    public void testGetAllEnrollments() {
        Student stu1 = new Student("Student1", "stu1", "s1@mail.com");
        Student stu2 = new Student("Student2", "stu2", "s2@mail.com");
        Student stu3 = new Student("Student3", "stu3", "s3@mail.com");
        Student stu4 = new Student("Student4", "stu4", "s4@mail.com");
        Seminar sem1 = new Seminar("sem1", new Course("", "", 0, ""), 20, new Instructor("", "", ""));
        Seminar sem2 = new Seminar("sem2", new Course("", "", 0, ""), 20, new Instructor("", "", ""));
        Seminar sem3 = new Seminar("sem3", new Course("", "", 0, ""), 20, new Instructor("", "", ""));
        Seminar sem4 = new Seminar("sem4", new Course("", "", 0, ""), 20, new Instructor("", "", ""));
        Enrollment enr1 = new Enrollment(stu1, sem1);
        Enrollment enr2 = new Enrollment(stu1, sem2);
        enr2.setGrade(82.1);
        enr2.setComplete(true);
        Enrollment enr3 = new Enrollment(stu1, sem4);
        enr3.setGrade(75.0);
        enr3.setComplete(true);
        Enrollment enr4 = new Enrollment(stu2, sem1);
        Enrollment enr5 = new Enrollment(stu2, sem3);
        Enrollment enr6 = new Enrollment(stu3, sem1);
        Enrollment enr7 = new Enrollment(stu3, sem4);
        enr7.setGrade(94.3);
        enr7.setComplete(true);
        Enrollment enr8 = new Enrollment(stu4, sem2);
        enr8.setGrade(55.0);
        enr8.setComplete(true);
        
        ArrayList<Enrollment> expected = new ArrayList<Enrollment>();
        
        expected.add(enr1);
        expected.add(enr2);
        expected.add(enr3);
        expected.add(enr4);
        expected.add(enr5);
        expected.add(enr6);
        expected.add(enr7);
        expected.add(enr8);
        
        assertTrue(compareEnrollmentArrayList(expected, enrollmentList.getAllEnrollments()));
    }
    
    @Test
    public void testContainsStuIdSemId() {
        assertTrue(enrollmentList.containsStuIdSemId("stu1", "sem1"));
        assertFalse(enrollmentList.containsStuIdSemId("stu4", "sem1"));
    }
    
    private boolean compareEnrollmentArrayList(ArrayList<Enrollment> list1, ArrayList<Enrollment> list2) { 
        if (list1.size() == list2.size()) {
            Collections.sort(list1);
            Collections.sort(list2);
            for (int i = 0; i < list1.size(); i++) {
                if (!list1.get(i).equalStuIdSemId(list2.get(i))) {
                    return false;
                }
            }
        } else {
            return false;
        }
        return true;
    }
}
