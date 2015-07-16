// A stack header in C++
// written using the Google Style Guide
//
// Sample usage:
// Stack stack;
// stack.Push(3);
// stack.Push(12);
// while(!stack.IsEmpty()) {
//   int n = stack.Pop() //n == 12 the first time, then n == 3
// }
// Note: Pop() will not throw exception if stack is empty,
// so make sure you know stack is not empty before popping.
// If stack is empty, pop will return 0

#ifndef STACK_H
#define STACK_H

class Stack {
  public:
    Stack();
    ~Stack();

    void Push(int);
    int Pop();
    bool IsEmpty();
	private:
		struct Node {
			int data;
			struct Node* next;
		};

    struct Node* last;
};

#endif
