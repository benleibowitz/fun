package com.ben.algofun;

/**
 * Quicksort class.
 */
public class Quicksort {
    /**
     * Sorts array of integers in place using quicksort.
     * @param array to be sorted
     */
    public static void sort(int[] array) {
        sort(array, 0, array.length - 1);
    }

    /**
     *
     * Recursively sorts array of integers in place using quicksort.
     * @param array to be sorted
     * @param leftIdx index at the end of the left subarray
     * @param rightIdx index at the beginning of the right subarray
     */
    private static void sort(int[] array, int leftIdx, int rightIdx) {
        if (leftIdx < rightIdx) {
            int splitIdx = partition(array, leftIdx, rightIdx);
            sort(array, leftIdx, splitIdx - 1);
            sort(array, splitIdx + 1, rightIdx);
        }
    }

    /**
     *
     * @param array to be sorted
     * @param leftIdx index at the end of the left subarray
     * @param rightIdx index at the beginning of the right subarray
     * @return the wall index, to the left of which elements are less than the pivot, and
     *  to the right of which elements are greater than the pivot
     */
    private static int partition(int[] array, int leftIdx, int rightIdx) {
        int pivot = array[rightIdx];
        int wallIdx = leftIdx - 1;

        for (int i = leftIdx; i < rightIdx; i++) {
            if (array[i] <= pivot) {
                wallIdx++;
                switchElements(array, wallIdx, i);
            }
        }

        switchElements(array, wallIdx + 1, rightIdx);
        return ++wallIdx;
    }

    /**
     * Take an array and switch elements at index1 and index2
     * @param array array to have elements switched in place
     * @param index1 index of item 1 to switch
     * @param index2 index of item 2 to switch
     */
    private static void switchElements(int[] array, int index1, int index2) {
        int val1 = array[index1];
        int val2 = array[index2];
        array[index1] = val2;
        array[index2] = val1;
    }
}
