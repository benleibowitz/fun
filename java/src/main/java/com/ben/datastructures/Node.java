package com.ben.datastructures;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Builder;
import lombok.Data;

/**
 * Use Lombok to generate getters, setters,
 * and builder pattern instead of constructor.
 * (You might need to install Lombok on your IDE to
 * remove the errors)
 */
@Data
@Builder
@JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
public class Node<E> {
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