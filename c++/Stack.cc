// A stack implementation in C++
// written using Google Style Guide for
// style conventions
// see Stack.h for usage
#include "Stack.h"
#include <iostream>
using std::cout;

Stack::Stack() {
	last = 0;
}

Stack::~Stack() {
  //Free all memory from node pointers
  while(last != 0) {
    Node* old = last;
    last = last->next;
    delete old;
  }
}

void Stack::Push(int data) {
  //Create new node in last position, and have it's next node be the old last node
	Node* old = last;
	last = new(struct Node);
	last->data = data;
	last->next = old;
}

int Stack::Pop() {
  //Pop() will return 0 if stack is empty
	if(last == 0)
		return 0;

	int data = last->data;
	last = last->next;
	return data;
}

bool Stack::IsEmpty() {
	return (last == 0);
}
