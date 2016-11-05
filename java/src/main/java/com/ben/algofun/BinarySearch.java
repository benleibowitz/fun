package com.ben.algofun;

/**
 * Created by ben on 9/23/16.
 */
public class BinarySearch {
    /**
     * Search returns the index of the given element in the given array if it is found.
     * If it is not found, it returns -1.
     * @param array
     * @param element
     * @param <E>
     * @return index of element, or -1 if not found
     */
    public static <E extends Comparable> int search(E[] array, E element) {
        if(array == null) {
            throw new IllegalArgumentException("Array cannot be null");
        }
        return search(array, element, 0, array.length - 1);
    }

    private static <E extends Comparable> int search(E[] array, E element, int startIdx, int endIdx) {
        int result = -1;

        //base case
        if(startIdx == endIdx) {
            return array[startIdx].equals(element) ? startIdx : result;
        } else if(startIdx < endIdx) {

            int middle = (endIdx + startIdx) / 2;

            if (array[middle].compareTo(element) > 0) {
                result = search(array, element, startIdx, middle - 1);
            } else if (array[middle].compareTo(element) < 0) {
                result = search(array, element, middle + 1, endIdx);
            } else {
                result = middle;
            }
        }

        return result;
    }

    /**
     * Use binary search, iteratively
     * @param array
     * @param searchKey
     * @param <E>
     * @return index of element, or -1 if not found
     */
    public static <E extends Comparable> int searchIterative(E[] array, int searchKey) {
        if(array == null) {
            throw new IllegalArgumentException("Null input array");
        }
        int foundIdx = -1;

        if(array.length > 0) {
            int lowerIdx = 0;
            int upperIdx = array.length - 1;
            int midIdx;
            boolean found = false;

            while(lowerIdx <= upperIdx && !found) {

                midIdx = (lowerIdx + upperIdx + 1) / 2;

                if (array[midIdx].compareTo(searchKey) > 0) {
                    upperIdx = midIdx - 1;
                } else if (array[midIdx].compareTo(searchKey) < 0) {
                    lowerIdx = midIdx + 1;
                } else {
                    foundIdx = midIdx;
                    found = true;
                }
            }

        }

        return foundIdx;
    }

}
