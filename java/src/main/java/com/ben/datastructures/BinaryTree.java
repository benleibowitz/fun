package com.ben.datastructures;

import lombok.Data;

public class BinaryTree<E extends Comparable> {
    /**
     * Left child <= current node.
     * Right child > current node.
     * @param <E>
     */
    @Data
    private class Node<E> {
        Node leftChildNode;
        Node rightChildNode;
        E data;

        public Node(E data) {
            this.data = data;
        }
    }
    
    Node headNode;
    int depth;
    
    public BinaryTree() {
        depth = 0;
    }
    
    public void add(E data) {
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

            if(currentDepth > depth) {
                depth = currentDepth;
            }
        }

    }
    
    public boolean contains(E data) {
        boolean contains = false;
        
        if(depth > 0) {
            contains = lookup(headNode, data);
        }

        return contains;
    }
    
    private boolean lookup(Node node, E data) {
        boolean contains = false;

        if(node != null) {
            switch(data.compareTo(node.getData())) {
                case 1:
                    contains = lookup(node.getRightChildNode(), data);
                    break;
                case -1:
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
