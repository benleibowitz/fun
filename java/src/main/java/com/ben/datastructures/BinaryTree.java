package com.ben.datastructures;

import lombok.Getter;

public class BinaryTree<E extends Comparable> {
    /**
     * Left child <= current node.
     * Right child > current node.
     * @param
     */
    private class Node {
        Node leftChildNode;
        Node rightChildNode;
        E data;

        public Node(E data) {
            this.data = data;
        }
    }
    
    Node root;

    @Getter
    int depth;
    
    public BinaryTree() {
        depth = 0;
    }
    
    public void add(E data) {
        add(data, root, null, 0);
    }

    private void add(E data, Node currentNode, Node parentNode, int currentDepth) {
        if(currentNode == null) {
            currentNode = new Node(data);

            //If currentNode is not a root node
            if(parentNode != null) {
                if (data.compareTo(parentNode.data) <= 0) {
                    //It's a left child
                    parentNode.leftChildNode = currentNode;
                } else {
                    parentNode.rightChildNode = currentNode;
                }
            } else {
                root = currentNode;
            }
        } else {
            if(data.compareTo(currentNode.data) <= 0) {
                add(data, currentNode.leftChildNode, currentNode, currentDepth + 1);
            } else {
                add(data, currentNode.rightChildNode, currentNode, currentDepth + 1);
            }
        }

        if(++currentDepth > depth) {
            depth = currentDepth;
        }
    }
    
    public boolean contains(E data) {
        return depth > 0 && lookup(root, data);
    }
    
    private boolean lookup(Node node, E data) {
        if(node != null) {
            if(data.compareTo(node.data) < 0) {
                return lookup(node.leftChildNode, data);
            } else if(data.compareTo(node.data) > 0) {
                return lookup(node.rightChildNode, data);
            } else {
                return true;
            }
        }
        return false;
    }
}
