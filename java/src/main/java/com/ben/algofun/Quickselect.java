package com.ben.algofun;

/**
 * Quickselect class
 */
public class Quickselect {

    /**
     * Selects the k^th smallest element. i.e., k = 2 selects the 2nd smallest element (i.e., item at index 1 in a sorted array)
     * @param array
     * @param k - the k^th smallest element to select
     * @return k^th smallest element
     */
    public static int select(int[] array, int k) {
        return select(array, 0, array.length - 1, k);
    }

    private static int select(int[] array, int leftIdx, int rightIdx, int k) {
        if(array.length == 0) {
            throw new IllegalArgumentException("Array length must be > 0");
        } else if(array.length < k) {
            throw new IllegalArgumentException("Idx must be less than or equal to array length");
        }

        if (leftIdx < rightIdx) {
            int splitIdx = partition(array, leftIdx, rightIdx);

            if (splitIdx == (k - 1)) {
                return array[splitIdx];
            } else if (splitIdx < (k - 1)) {
                return select(array, splitIdx + 1, rightIdx, k);
            } else {
                return select(array, leftIdx, splitIdx - 1, k);
            }
        }

        return array[leftIdx];
    }

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
