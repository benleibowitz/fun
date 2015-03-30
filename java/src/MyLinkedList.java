public class MyLinkedList {
	
	private class Node {
		private Object object;
		private Node nextNode;
		
		Node(Object object) {
			this.object = object;
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
	}
	
	private int size;
	private Node firstNode;
	
	public MyLinkedList() {
		firstNode = null;
		size = 0;
	}
	
	public void add(Object element) {
		if(firstNode == null) {
			firstNode = new Node(element);
		} else {
			Node currentNode = firstNode;
			
			while(currentNode.getNextNode() != null) {
				currentNode = currentNode.getNextNode();
			}
			
			currentNode.setNextNode(new Node(element));
		}
		
		size++;
	}
	
	public void remove(int index) {
		if(index >= size || index < 0)
			throw new IndexOutOfBoundsException();
		
		Node currentNode = firstNode;
		
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
		
		Node currentNode = firstNode;
		
		for(int i = 0; i < index; i++) {
			currentNode = currentNode.getNextNode();
		}
		
		return currentNode.getObject();
	}
	
	public void clear() {
		Node currentNode = firstNode;
		
		while(currentNode.getNextNode() != null) {
			currentNode.setNextNode(null);
		}
		
		firstNode = null;
		currentNode = null;
		
		size = 0;
	}

	
	public void print() {
		Node currentNode = firstNode;
		
		while(currentNode != null) {
			System.out.println(currentNode.getObject());
			currentNode = currentNode.getNextNode();
		}
	}
	
	@Override
	public String toString() {
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append("[");
		
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
	
	public int size() {
		return size;
	}
}
