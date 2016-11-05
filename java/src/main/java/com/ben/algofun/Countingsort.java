package com.ben.algofun;

/**
 * Created by ben on 10/24/16.
 */
public class Countingsort {
    /**
     * Will find minimum and maximum element of array, then
     * perform counting sort
     * @param input
     * @return sorted array
     */
    public static int[] sort(int[] input) {
        if(input == null) {
            throw new IllegalArgumentException("Input cannot be null");
        } else if(input.length < 2) {
            return input;
        }

        Integer max = input[0];
        Integer min = input[0];
        for(int item : input) {
            if(item > max) {
                max = item;
            } else if(item < min) {
                min = item;
            }
        }

        return sort(input, min, max);
    }

    /**
     * Sort array using the given range min and max
     * @param input array
     * @param min minimum element in array
     * @param max maximum element in array
     * @return sorted array
     */
    public static int[] sort(int[] input, int min, int max) {
        int[] counts = new int[max - min + 1];
        for(int i = 0; i < input.length; i++) {
            counts[input[i] - min]++;
        }

        int[] result = new int[input.length];
        int resultIdx = 0;
        for(int i = 0; i < counts.length; i++) {
            for(int j = 0; j < counts[i]; j++) {
                result[resultIdx++] = i + min;
            }
        }
        return result;
    }
}
