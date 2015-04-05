package alg;

import java.math.BigDecimal;

public class BinaryTree {
	private class Node {
		//LeftChildNode will be <= current node,
		//and rightChildNode will be > current Node
		Node leftChildNode;
		Node rightChildNode;
		BigDecimal data;
		
		Node(BigDecimal data) {
			this.data = data;
		}
		
		public void setLeftChildNode(Node newNode) {
			leftChildNode = newNode;
		}
		
		public void setRightChildNode(Node newNode) {
			rightChildNode = newNode;
		}
		
		public Node getLeftChildNode() {
			return leftChildNode;
		}

		public Node getRightChildNode() {
			return rightChildNode;
		}

		private BigDecimal getData() {
			return data;
		}
	}
	
	Node headNode;
	int depth;
	
	public BinaryTree() {
		depth = 0;
	}
	
	public void add(BigDecimal data) {
		if(headNode == null) {
			headNode = new Node(data);
			depth++;
		} else {
			int currentDepth = 1;
			Node currentNode = headNode;
			
			boolean placedNode = false;
			while(!placedNode) {
				currentDepth++;

				if(data.compareTo(currentNode.getData()) == 1) {
					//New node is bigger than currentNode, set on right side if
					//right side is null. else, traverse the right child node
					if(currentNode.getRightChildNode() == null) {
						currentNode.setRightChildNode(new Node(data));
						placedNode = true;
					} else {
						currentNode = currentNode.getRightChildNode();
					}
				} else {
					//New node is <= currentNode, set on left side 
					if(currentNode.getLeftChildNode() == null) {
						currentNode.setLeftChildNode(new Node(data));
						placedNode = true;
					} else {
						currentNode = currentNode.getLeftChildNode();
					}
					
				}
			}

			if(currentDepth > depth)
				depth = currentDepth;
		}

	}
	
	public boolean contains(BigDecimal data) {
	    boolean contains = false;
	    
	    if(depth > 0) {
	        Node currentNode = headNode;
	        
    	    if(currentNode.getData().compareTo(data) == 0) {
    	        contains = true;
    	    } else {
    	        while(currentNode != null && !contains) {
    	            
    	            switch(data.compareTo(currentNode.getData())) {
    	                case 1:
                            currentNode = currentNode.getRightChildNode();
    	                    break;
    	                case -1:
                            currentNode = currentNode.getLeftChildNode();
    	                    break;
    	                default:
                            contains = true;
    	            }
    	            
                }
    	    }
	    }
	    
	    return contains;
	}
	
	public int getDepth() {
		return depth;
	}
	
}
