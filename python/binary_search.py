'''
Created on Mar 13, 2015

@author: Ben Leibowitz
'''

def binary_search(in_array, key):
    if in_array is None:
        raise TypeError
    
    return _recurse(in_array, 0, len(in_array) - 1, key)
    
def _recurse(in_array, low_idx, hi_idx, key):
    found_idx = -1
    
    mid = int((low_idx + hi_idx) / 2)
    
    if low_idx <= hi_idx:
        if in_array[mid] > key:
            found_idx = _recurse(in_array, low_idx, mid - 1, key)
        elif in_array[mid] < key:
            found_idx = _recurse(in_array, mid + 1, hi_idx, key)
        else:
            found_idx = mid
        
    return found_idx
