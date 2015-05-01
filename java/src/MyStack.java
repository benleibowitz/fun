package com;


public class MyStack {
	private class Node {
		private Node nextNode;
		private Object data;
		
		Node(Object data) {
			nextNode = null;
			this.data = data;
		}
		
		private Node getNextNode() {
			return nextNode;
		}
		
		private void setNextNode(Node nextNode) {
			this.nextNode = nextNode;
		}
		
		private Object getData() {
			return data;
		}
	}
	
	private Node firstNode;
	private int size;
	
	public MyStack() {
	}
	
	public void add(Object obj) {
		Node currentFirst = firstNode;
		firstNode = new Node(obj);
		
		if(firstNode != null) {
			firstNode.setNextNode(currentFirst);
		}
		
		size++;
	}
	
	public Object pop() {
		if(size == 0)
			throw new ArrayIndexOutOfBoundsException("Cannot pop from empty stack");
		
		Node oldFirstNode = firstNode;
		firstNode = firstNode.getNextNode();
		
		size--;
		return oldFirstNode.getData();
	}
	
	public int size() {
		return size;
	}

	
}
