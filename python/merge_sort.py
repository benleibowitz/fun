'''

Merge sort implementation in Python.

Note that this only sorts lists of numbers
(floats, ints, etc). It will raise TypeError
if there are strings, dictionaries, generators,
or None types.

Implementation has been benchmarked for performance
to decide which approaches are best.
ie:
    - "math.ceil(len(ary) / 2)" vs. "len(ary) // 2" vs. 
        vs. "int( len(ary) / 2)" vs. if/else statement for finding mid-point
    - initializing full list and indexing into it vs. initializing empty list and appending.. (indexing
        is about 7% faster is # elements gets large)

'''

def merge_sort(input_array):
    if len(input_array) > 1:
        mid = len(input_array) // 2

        l_array = input_array[:mid]
        r_array = input_array[mid:]

        l_array = merge_sort(l_array)
        r_array = merge_sort(r_array)

        new_array = _merge_2_ar(l_array, r_array)
    else:
        new_array = input_array

    return new_array

def _merge_2_ar(l_array, r_array):
    main_len = len(l_array) + len(r_array)
    
    new_ar = [None] * main_len
    
    l_idx = 0
    r_idx = 0
    
    for i in range(main_len):
        place_l_val = False
        place_r_val = False
        
        if l_idx < len(l_array) and r_idx < len(r_array):
            if l_array[l_idx] < r_array[r_idx]:
                place_l_val = True
            else:
                place_r_val = True
        elif l_idx < len(l_array):
            place_l_val = True
        else:
            place_r_val = True
            
            
        if place_l_val:
            new_ar[i] = l_array[l_idx]
            l_idx += 1
        elif place_r_val:
            new_ar[i] = r_array[r_idx]
            r_idx += 1

    return new_ar
