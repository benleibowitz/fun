package com.ben.datastructures;
public class MyLinkedList<E> {
	
	private class Node<E> {
		private E data;
		private Node<E> nextNode;
		
		Node(E object) {
			this.data = object;
		}
		
		private void setNextNode(Node<E> nextNode) {
			this.nextNode = nextNode;
		}
		
		private E getObject() {
			return data;
		}
		
		private Node<E> getNextNode() {
			return nextNode;
		}
	}
	
	private int size;
	private Node<E> firstNode;
	
	public MyLinkedList() {
		firstNode = null;
		size = 0;
	}
	
	public void add(E element) {
		if(firstNode == null) {
			firstNode = new Node<>(element);
		} else {
			Node<E> currentNode = firstNode;
			
			while(currentNode.getNextNode() != null) {
				currentNode = currentNode.getNextNode();
			}
			
			currentNode.setNextNode(new Node<>(element));
		}
		
		size++;
	}
	
	public void remove(int index) {
		if(index >= size || index < 0)
			throw new IndexOutOfBoundsException();
		
		Node<E> currentNode = firstNode;
		
		if(index == 0) {
			firstNode = firstNode.getNextNode();
		} else {
			
			for(int i = 0; i < index-1; i++) {
				currentNode = currentNode.getNextNode();
			}

			currentNode.setNextNode(currentNode.getNextNode().getNextNode());
		}
		
		size--;
	}
	
	public Object get(int index) {
		if(index >= size || index < 0)
			throw new IndexOutOfBoundsException();
		
		Node<E> currentNode = firstNode;
		
		for(int i = 0; i < index; i++) {
			currentNode = currentNode.getNextNode();
		}
		
		return currentNode.getObject();
	}
	
	public void clear() {
		Node<E> currentNode = firstNode;
		
		while(currentNode.getNextNode() != null) {
			currentNode.setNextNode(null);
		}
		
		firstNode = null;
		currentNode = null;
		
		size = 0;
	}

	
	public void print() {
		Node<E> currentNode = firstNode;
		
		while(currentNode != null) {
			System.out.println(currentNode.getObject());
			currentNode = currentNode.getNextNode();
		}
	}
	
	@Override
	public String toString() {
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append("[");
		
		Node<E> currentNode = firstNode;
		
		for(int i = 0; i < size; i++) {
			stringBuilder.append(currentNode.getObject());
			currentNode = currentNode.getNextNode();
			
			if(i < size-1)
				stringBuilder.append(", ");
		}
		
		stringBuilder.append("]");
		
		return stringBuilder.toString();
	}
	
	public int size() {
		return size;
	}
}
