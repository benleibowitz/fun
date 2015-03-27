package com;

public class TwoWayLinkedList {
	
	private class Node {
		private Object object;
		private Node nextNode;
		private Node prevNode;
		
		Node(Object object) {
			nextNode = null;
			prevNode = null;
			this.object = object;
		}
		
		private void setPrevNode(Node prevNode) {
			this.prevNode = prevNode;
		}
		
		private void setNextNode(Node nextNode) {
			this.nextNode = nextNode;
		}
		
		private Object getObject() {
			return object;
		}
		
		private Node getNextNode() {
			return nextNode;
		}
		
		private Node getPrevNode() {
			return prevNode;
		}
	}
	
	private int size;
	private Node firstNode;
	private Node lastNode;
	
	public TwoWayLinkedList() {
		firstNode = null;
		lastNode = null;
		size = 0;
	}
	
	public void add(Object element) {
		Node newNode = new Node(element);

		if(firstNode == null) {
			firstNode = newNode;
			lastNode = firstNode;
		} else {
			lastNode.setNextNode(newNode);
			
			Node oldLastNode = lastNode;
			
			newNode.setPrevNode(oldLastNode);
			
			lastNode = newNode;

		}

		size++;
	}
	/*
	public void insert(int index, Object object) {
		if(index > size || index < 0)
			throw new IndexOutOfBoundsException();
		
		Node newNode = new Node(object);
		Node currentNode = firstNode;
		
		if(index == 0) {
			currentNode = firstNode;
			firstNode = newNode;
			firstNode.setNextNode(currentNode);
		} else {
			for(int i = 0; i < (index-1); i++) {
				currentNode = currentNode.getNextNode();
			}
			
			Node nextNode = currentNode.getNextNode();
			currentNode.setNextNode(newNode);
			newNode.setNextNode(nextNode);
		}
		
		size++;
	}
	*/
	public void remove(int index) {
		if(index >= size || index < 0)
			throw new IndexOutOfBoundsException();
		
		Node currentNode = firstNode;
		if(index == 0 && size == 1) {
			firstNode = null;
			lastNode = null;
		}else if(index == 0) {
			firstNode = firstNode.getNextNode();
			firstNode.setPrevNode(null);
		} else if(index < (size/2)) {

			for(int i = 0; i < index - 1; i++) {
				currentNode = currentNode.getNextNode();
			}
			
			currentNode.setNextNode(currentNode.getNextNode().getNextNode());
			currentNode.getNextNode().setPrevNode(currentNode);
			
		} else {
			currentNode = lastNode;
			
			for(int i = size - 1; i >= index; i--) {
				currentNode = currentNode.getPrevNode();
			}
			
			if(index < size - 1) {
				currentNode.setNextNode(currentNode.getNextNode().getNextNode());
				currentNode.getNextNode().setPrevNode(currentNode);
			} else {
				currentNode.setNextNode(null);
				lastNode = currentNode;
			}
			
		}
		
		size--;
	}
	
	public Object get(int index) {
		if(index >= size || index < 0)
			throw new IndexOutOfBoundsException();
		
		Node currentNode;
		
		if(index < (size/2)) {
			//Index in from the left side
			currentNode = firstNode;
			
			for(int i = 0; i < index; i++) {
				currentNode = currentNode.getNextNode();
			}

		} else {
			//Index in from the right side
			currentNode = lastNode;
			
			for(int i = size - 1; i > index; i--) {
				currentNode = currentNode.getPrevNode();
			}
		}
		
		return currentNode.getObject();
	}
	
	public void clear() {

		firstNode = null;
		lastNode = null;
		
		size = 0;
	}

	public int size() {
		return size;
	}

	public boolean isEmpty() {
		return (size == 0);
	}
	
	public void print() {
		Node currentNode = firstNode;
		
		while(currentNode != null) {
			System.out.print(currentNode.getObject()+" ");
			currentNode = currentNode.getNextNode();
		}
		
		System.out.println("");
	}
	
	@Override
	public String toString() {
		StringBuilder stringBuilder = new StringBuilder("[");
		
		Node currentNode = firstNode;
		
		for(int i = 0; i < size; i++) {
			stringBuilder.append(currentNode.getObject());
			currentNode = currentNode.getNextNode();
			
			if(i < size-1)
				stringBuilder.append(", ");
		}
		
		stringBuilder.append("]");
		
		return stringBuilder.toString();
	}

}


