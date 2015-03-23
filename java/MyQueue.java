package com;

public class MyQueue {
	
	//Each node contains a reference to the next node,
	//and the data being stored in that node
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
	
	public MyQueue() {
		firstNode = null;
		size = 0;
	}
	
	public void add(Object data) {
		if(size == 0) {
			firstNode = new Node(data);
		} else {

			Node currentNode = firstNode;
			
			for(int i = 0; i < size - 1; i++) {
				currentNode = currentNode.getNextNode();
			}
			
			currentNode.setNextNode(new Node(data));
		}
		
		size++;
	}
		
	public Object pop() {
		if(size == 0)
			throw new IndexOutOfBoundsException("Cannot remove from empty queue");
		
		Object returnData = firstNode.getData();
		
		firstNode = firstNode.getNextNode();
		
		size--;
		
		return returnData;
	}
	
	public int size() {
		return size;
	}
	
	
	
}
