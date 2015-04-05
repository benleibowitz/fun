'''
Created on Apr 5, 2015

@author: ben
'''

class MyCounter:
    def __init__(self, input_struct=None):
        self.__name__ = 'MyCounter'
        self.count_dict = {}
        
        if input_struct is not None:
            self.update(input_struct)
        
    def __add_dict__(self, dict_in):
        for key in dict_in:
            self.count_dict[key] = self.count_dict.get(key, 0) + dict_in[key]
                
    def __add_list__(self, list_in):
        for item in list_in:
                self.count_dict[item] = self.count_dict.get(item, 0) + 1
                
    def __add_primative__(self, primative_in):
        self.count_dict[primative_in] = self.count_dict.get(primative_in, 0) + 1
        
    def __str__(self):
        return 'MyCounter({})'.format(self.count_dict)
    
    def __len__(self):
        return len(self.count_dict) 
    
    def __setitem__(self, a, b):
        raise AttributeError('{} has no __setitem__ method. Please use update method instead'.format(self.__name__))
    
    def __getitem__(self, item):
        return self.count_dict[item]
    
    def keys(self):
        return self.count_dict.keys()
    
    def update(self, data):
        self.method_mapping = {dict: self.__add_dict__,
                          list: self.__add_list__,
                          str: self.__add_primative__,
                          int: self.__add_primative__,
                          float: self.__add_primative__}
        
        if not type(data) in self.method_mapping.keys():
            raise ValueError
        
        if data is not None:
            self.method_mapping[type(data)](data)
