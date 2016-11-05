package com.ben.datastructures;

import lombok.Getter;

public class MyLinkedList<E> {
	private class Node {
		private E element;
		private Node nextNode;
		
		Node(E element) {
			this.element = element;
		}
	}

	@Getter
	private int size;
	private Node firstNode;
	
	public MyLinkedList() {
	}
	
	public void add(E element) {
		if(firstNode == null) {
			firstNode = new Node(element);
		} else {
			Node currentNode = firstNode;
			
			while(currentNode.nextNode != null) {
				currentNode = currentNode.nextNode;
			}
			
			currentNode.nextNode = new Node(element);
		}
		
		size++;
	}
	
	public boolean contains(E element) {
		return (get(element) != -1);
	}
	
	public void remove(int index) {
		if(index >= size || index < 0) {
			throw new IndexOutOfBoundsException();
		}
		
		Node currentNode = firstNode;
		
		if(index == 0) {
			firstNode = firstNode.nextNode;
		} else {
			
			for(int i = 0; i < index-1; i++) {
				currentNode = currentNode.nextNode;
			}

			currentNode.nextNode = currentNode.nextNode.nextNode;
		}
		
		size--;
	}
	
	public int get(E element) {
		int index = -1;
		
		Node currentNode = firstNode;
		
		int count = 0;
		while(currentNode != null && index == -1) {
			E currentElement = currentNode.element;
			
			if(currentElement == null) {
				if(element == null) {
					index = count;
				}
			} else if(currentElement.equals(element)) {
				index = count;
			}

			currentNode = currentNode.nextNode;
			count++;
		}
		
		return index;
	}
	
	public E get(int index) {
		if(index >= size || index < 0) {
			throw new IndexOutOfBoundsException();
		}
		
		Node currentNode = firstNode;
		
		for(int i = 0; i < index; i++) {
			currentNode = currentNode.nextNode;
		}
		
		return currentNode.element;
	}
	
	public void clear() {
		Node currentNode = firstNode;
		
		while(currentNode.nextNode != null) {
			currentNode.nextNode = null;
		}
		
		firstNode = null;
		currentNode = null;
		
		size = 0;
	}
	
	@Override
	public String toString() {
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append("[");
		
		Node currentNode = firstNode;
		
		for(int i = 0; i < size; i++) {
			stringBuilder.append(currentNode.element);
			currentNode = currentNode.nextNode;
			
			if(i < size-1) {
				stringBuilder.append(", ");
			}
		}
		
		stringBuilder.append("]");
		
		return stringBuilder.toString();
	}
}
