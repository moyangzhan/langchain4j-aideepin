package com.moyz.adi.common.util;

import java.util.Collection;

public class AdiAssert {

    private AdiAssert() {
        throw new AssertionError("Cannot instantiate AdiAssert class.");
    }

    // Assert that a condition is true, throws IllegalArgumentException if false
    public static void isTrue(boolean condition, String message) {
        if (!condition) {
            throw new IllegalArgumentException(message);
        }
    }

    // Assert that a condition is false, throws IllegalArgumentException if true
    public static void isFalse(boolean condition, String message) {
        if (condition) {
            throw new IllegalArgumentException(message);
        }
    }

    // Assert that an object is not null, throws NullPointerException if null
    public static void isNotNull(Object object, String message) {
        if (object == null) {
            throw new NullPointerException(message);
        }
    }

    // Assert that a string is not empty or null
    public static void isNotEmpty(String str, String message) {
        if (str == null || str.isEmpty()) {
            throw new IllegalArgumentException(message);
        }
    }

    // Assert that a collection is not empty or null
    public static <T> void isNotEmpty(Collection<T> collection, String message) {
        if (collection == null || collection.isEmpty()) {
            throw new IllegalArgumentException(message);
        }
    }

    // Assert that two objects are equal
    public static void isEquals(Object expected, Object actual, String message) {
        if (!expected.equals(actual)) {
            throw new IllegalArgumentException(message + " Expected: " + expected + ", but was: " + actual);
        }
    }

    // Assert that an array has a specific length
    public static void isArrayLength(Object[] array, int length, String message) {
        if (array == null || array.length != length) {
            throw new IllegalArgumentException(message + " Expected length: " + length + ", but was: " + (array == null ? "null" : array.length));
        }
    }

    // Assert that a value is within a certain range (inclusive)
    public static void isInRange(int value, int min, int max, String message) {
        if (value < min || value > max) {
            throw new IllegalArgumentException(message + " Value: " + value + " is out of range [" + min + ", " + max + "].");
        }
    }
}
