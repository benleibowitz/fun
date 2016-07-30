package com.ben.datastructures;


public class MyStack<E> {
    private class Node {
        private Node nextNode;
        private E data;
        
        Node(E data) {
            nextNode = null;
            this.data = data;
        }
        
        private Node getNextNode() {
            return nextNode;
        }
        
        private void setNextNode(Node nextNode) {
            this.nextNode = nextNode;
        }
        
        private E getData() {
            return data;
        }
    }
    
    private Node firstNode;
    private int size;
    
    public MyStack() {
    }
    
    public void add(E obj) {
        Node newNode = new Node(obj);
        newNode.setNextNode(firstNode);
        firstNode = newNode;
        
        size++;
    }
    
    public E pop() {
        if(isEmpty()) {
            throw new ArrayIndexOutOfBoundsException("Cannot pop from empty stack");
        }

        Node oldFirstNode = firstNode;
        firstNode = firstNode.getNextNode();
        
        size--;
        return oldFirstNode.getData();
    }
    
    public int size() {
        return size;
    }
    
    public boolean isEmpty() {
        return (size == 0);
    }

    
}
