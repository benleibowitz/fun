package com.ben.algofun;

/**
 * Created by Ben on 11/5/2016.
 */
public class InsertSort {
    /**
     * Sorts array in place
     * @param inArr
     */
    public static void insertSort(int[] inArr) {
        for(int i = 1; i < inArr.length; i++) {
            int key = inArr[i];

            boolean inserted = false;
            for(int j = i; j > 0 && !inserted; j--) {
                int prevKey = inArr[j-1];

                if(prevKey > key) {
                    inArr[j] = prevKey;
                } else {
                    inArr[j] = key;
                    inserted = true;
                }
            }

            if(!inserted) {
                inArr[0] = key;
            }
        }
    }
}
