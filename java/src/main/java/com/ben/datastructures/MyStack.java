package com.ben.datastructures;


import lombok.Getter;

public class MyStack<E> {
    private class Node {
        private Node nextNode;
        private E data;
        
        Node(E data) {
            nextNode = null;
            this.data = data;
        }
    }
    
    private Node firstNode;

    @Getter
    private int size;
    
    public MyStack() {
    }
    
    public void add(E obj) {
        Node newNode = new Node(obj);
        newNode.nextNode = firstNode;
        firstNode = newNode;
        
        size++;
    }
    
    public E pop() {
        if(isEmpty()) {
            throw new ArrayIndexOutOfBoundsException("Cannot pop from empty stack");
        }

        Node oldFirstNode = firstNode;
        firstNode = firstNode.nextNode;
        
        size--;
        return oldFirstNode.data;
    }
    
    public boolean isEmpty() {
        return (size == 0);
    }

    
}
