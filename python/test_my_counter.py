'''
Created on Apr 5, 2015

@author: ben
'''
import unittest
from collections import Counter
from my_counter import MyCounter

class TestCase(unittest.TestCase):
    def setUp(self):
        self.counter = Counter()
        self.my_counter = MyCounter()
        self.str_list = ['foo', 'bar', 'foo', 'bar', 'foo', 'abc', 'test', 'my', 'list']
        self.num_list = [1,2,3,4,5,6,7,1.2,1.111]
        self.assort_list = [1,2,3,4,'a','b','c','d',1.2,2.3,3.4,4.5]
        self.fail_list = [1,2,3,4,'a','b', ['1', 3], {}]
        self.str_dict = {'foo':1, 'bar':3, 'what':14, 'my':34, 'fox':82}
       
    def testStrList(self):
        self.counter.update(self.str_list)
        self.my_counter.update(self.str_list)
        
        for item in self.counter:
            self.assertEqual(self.counter[item], self.my_counter[item], 'Not equal for:{}'.format(item))
            
    def testNumList(self):
        self.counter.update(self.num_list)
        self.my_counter.update(self.num_list)
        
        for item in self.counter:
            self.assertEqual(self.counter[item], self.my_counter[item], 'Not equal for:{}'.format(item))            
    
    def testAssortList(self):
        self.counter.update(self.assort_list)
        self.my_counter.update(self.assort_list)
        
        for item in self.counter:
            self.assertEqual(self.counter[item], self.my_counter[item], 'Not equal for:{}'.format(item))
     
    def testFailList(self):
        self.assertRaises(TypeError, self.my_counter.update, self.fail_list)
        
    def testItems(self):
        self.my_counter.update(self.str_dict)
        self.assertEquals(self.str_dict.keys(), self.my_counter.keys())
            
    
if __name__ == '__main__':
    unittest.main()
