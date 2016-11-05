package com.ben.algofun;

import java.util.Arrays;

/**
 * Created by ben on 10/27/16.
 */
public class KMin {
    /**
     * Find k min elements. If k > array length, return all elements in array.
     * Returns unsorted.
     * @param array
     * @param k
     * @return
     */
    public static <E extends Comparable> E[] findKMin(E[] array, int k) {
        if(k <= 0) {
            throw new IllegalArgumentException("k must be > 0");
        }
        int kOrMaxLen = k > array.length ? array.length : k;
        E[] kMin = Arrays.copyOf(array, kOrMaxLen);

        E kElmt = Quickselect.select(array, kOrMaxLen);

        int kIdx = 0;
        for(int i = 0; i < kMin.length; i++) {
            if(array[i].compareTo(kElmt) < 0) {
                kMin[kIdx++] = array[i];
            }
        }

        while(kIdx < kMin.length) {
            kMin[kIdx++] = kElmt;
        }

        return kMin;
    }
}
