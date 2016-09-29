package com.ben.algofun;

/**
 * Created by ben on 9/23/16.
 */
public class BinarySearch {
    /**
     * Search returns the index of the given element in the given array if it is found.
     * If it is not found, it returns -1.
     */
    public static <E extends Comparable> int search(E[] array, int element) {
        return search(array, element, 0, array.length - 1);
    }

    private static <E extends Comparable> int search(E[] array, int element, int startIdx, int endIdx) {
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

}
