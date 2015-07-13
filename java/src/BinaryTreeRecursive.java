package alg;

import java.math.BigDecimal;

import org.junit.Assert;

public class BinaryTreeRecursive {
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
    
    public BinaryTreeRecursive() {
        depth = 0;
    }
    
    public void add(BigDecimal data) {
        if(headNode == null) {
            headNode = new Node(data);
            depth++;
        } else {
            add(null, headNode, data, 1);
        }
    }
    
    private void add(Node parentNode, Node currentNode, BigDecimal data, int currentDepth) {
        if(currentNode == null) {
            currentNode = new Node(data);
            
            switch(parentNode.getData().compareTo(data)) {
                case -1:
                    parentNode.setRightChildNode(currentNode);
                    break;
                case 1:
                    parentNode.setLeftChildNode(currentNode);
                    break;
                default:
                    throw new IllegalArgumentException("Duplicate element cannot be added for: " + data);
            }
            
            if(currentDepth > depth)
                depth = currentDepth;
        } else {
            switch(currentNode.getData().compareTo(data)) {
                case -1:
                    add(currentNode, currentNode.getRightChildNode(), data, currentDepth + 1);
                    break;
                case 1:
                    add(currentNode, currentNode.getLeftChildNode(), data, currentDepth + 1);
                    break;
                default:
                    throw new IllegalArgumentException("Duplicate element cannot be added for: " + data);
            }
        }
    }
    
    public boolean contains(BigDecimal data) {
        boolean contains = false;
        
        if(data == null)
            throw new IllegalArgumentException("Data cannot be null");
        
        if(depth > 0)
            contains = lookup(headNode, data);
        
        return contains;
    }
    
    private boolean lookup(Node node, BigDecimal data) {
        boolean contains = false;

        if(node != null) {
            switch(node.getData().compareTo(data)) {
                case -1:
                    contains = lookup(node.getRightChildNode(), data);
                    break;
                case 1:
                    contains = lookup(node.getLeftChildNode(), data);
                    break;
                default:
                    contains = true;
            }
        }
        
        return contains;
    }
    
    public int getDepth() {
        return depth;
    }
    
    
}
