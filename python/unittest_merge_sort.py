'''
    Unit tests to test my implementation
    of merge_sort in Python.
'''
from merge_sort import merge_sort
import random
import unittest

class TestCase(unittest.TestCase):
    def setUp(self):
        print('Setting up')
        self.array1 = [random.randint(-100, 100) for _ in range(20)]
        self.array2a = [random.randint(5, 10) for _ in range(20)]
        self.array2b = [random.randint(-10, -5) for _ in range(20)]
        self.array3 = [8]
        self.array4 = [[5], [3]]
        self.array5 = []
        self.array6 = None
        self.array7 = [1, 54, 'non integer']
        self.array8 = [random.randint(-10000, 10000) for _ in range(1000000)]
        self.array9 = [3, 4.298374109283, 3209.0912991, -999283.00981285]
        self.array10 = [0, 0, 0, 0, 0, 0, 0, 0]
        self.array11 = (random.randint(-100, 100) for _ in range(20))
        
 
    def tearDown(self):
        print('Tearing down')
        
    def testPositiveAndNegative(self):
        self.assertEqual(merge_sort(self.array1), sorted(self.array1))
        
    def testPositiveOnly(self):
        self.assertEqual(merge_sort(self.array2a), sorted(self.array2a))
        
    def testNegativeOnly(self):
        self.assertEqual(merge_sort(self.array2b), sorted(self.array2b))
        
    def testListLen1(self):
        self.assertEqual(merge_sort(self.array3), self.array3)
        
    def testNestedLists(self):
        self.assertEqual(merge_sort(self.array4), sorted(self.array4))

    def testEmptyArray(self):
        self.assertEqual(merge_sort(self.array5), [])
        
    def testNone(self):
        self.assertRaises(TypeError, merge_sort, self.array6)
        
    def testNonInteger(self):
        self.assertRaises(TypeError, merge_sort, self.array7)
        
    def testLargeArray(self):
        self.assertEqual(merge_sort(self.array8), sorted(self.array8))
        
    def testFloats(self):
        self.assertEqual(merge_sort(self.array9), sorted(self.array9))
        
    def testDuplicates(self):
        self.assertEqual(merge_sort(self.array10), sorted(self.array10))
    
    def testGenerator(self):
        self.assertRaises(TypeError, merge_sort, self.array11)
        
if __name__ == '__main__':
    unittest.main()
