package info.kgeorgiy.ja.leontev.student;

import info.kgeorgiy.java.advanced.student.GroupName;
import info.kgeorgiy.java.advanced.student.Student;
import info.kgeorgiy.java.advanced.student.StudentQuery;

import java.util.*;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.stream.Collectors;


public class StudentDB implements StudentQuery {
    private static final Comparator<Student> STUDENT_COMPARATOR = Comparator.comparing(Student::getLastName)
            .thenComparing(Student::getFirstName)
            .reversed()
            .thenComparing(Student::getId);

    private static <R> List<R> getStudentProperties(final List<Student> students,
                                                    final Function<Student, R> mapper) {
        return students.stream().map(mapper).collect(Collectors.toList());
    }

    private static List<Student> getSortedProperties(final Collection<Student> students,
                                                     final Comparator<Student> cmp) {
        return students.stream().sorted(cmp).collect(Collectors.toList());
    }

    private static <T> List<Student> findStudentsBy(final Collection<Student> students,
                                                    final Function<Student, T> func, T name) {
        return students.stream()
                .filter(s -> func.apply(s).equals(name))
                .sorted(STUDENT_COMPARATOR)
                .collect(Collectors.toList());
    }

    @Override
    public List<String> getFirstNames(final List<Student> students) {
        return getStudentProperties(students, Student::getFirstName);
    }

    @Override
    public List<String> getLastNames(final List<Student> students) {
        return getStudentProperties(students, Student::getLastName);
    }

    @Override
    public List<GroupName> getGroups(final List<Student> students) {
        return getStudentProperties(students, Student::getGroup);
    }

    @Override
    public List<String> getFullNames(final List<Student> students) {
        return getStudentProperties(students, s -> String.format("%s %s", s.getFirstName(), s.getLastName()));
    }

    @Override
    public List<Student> sortStudentsById(final Collection<Student> students) {
        return getSortedProperties(students, Comparator.comparingInt(Student::getId));
    }

    @Override
    public List<Student> sortStudentsByName(final Collection<Student> students) {
        return getSortedProperties(students, STUDENT_COMPARATOR);
    }

    @Override
    public List<Student> findStudentsByFirstName(final Collection<Student> students, String name) {
        return findStudentsBy(students, Student::getFirstName, name);
    }

    @Override
    public List<Student> findStudentsByLastName(final Collection<Student> students, String name) {
        return findStudentsBy(students, Student::getLastName, name);
    }

    public List<Student> findStudentsByGroup(final Collection<Student> students, GroupName group) {
        return findStudentsBy(students, Student::getGroup, group);
    }

    @Override
    public Set<String> getDistinctFirstNames(final List<Student> students) {
        return students.stream()
                .map(Student::getFirstName)
                .collect(Collectors.toCollection(TreeSet::new));
    }

    @Override
    public String getMaxStudentFirstName(final List<Student> students) {
        return students.stream()
                .max(Comparator.comparingInt(Student::getId))
                .map(Student::getFirstName)
                .orElse("");
    }

    public Map<String, String> findStudentNamesByGroup(final Collection<Student> students, GroupName group) {
        return findStudentsByGroup(students, group).stream()
                .collect(Collectors.toMap(Student::getLastName, Student::getFirstName, BinaryOperator
                        .minBy(Comparator.naturalOrder())));
    }
}
