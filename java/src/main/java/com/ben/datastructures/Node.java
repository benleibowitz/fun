package com.ben.datastructures;

import java.io.Serializable;

import lombok.Builder;
import lombok.Data;

/**
 * Use Lombok to generate getters, setters,
 * and builder pattern instead of constructor
 */
@Data
@Builder
public class Node<E> implements Serializable {

    /**
     *  Serializable
     */
    private static final long serialVersionUID = -2791375802729648470L;
    
    /**
     * Data contained in Node
     */
    private E data;
    
    /**
     * Right node
     */
    private Node<E> rightNode;
    
    /**
     * Left node
     */
    private Node<E> leftNode;
    
}