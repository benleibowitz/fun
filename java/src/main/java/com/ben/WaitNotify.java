package com.ben;
/*
 * 
 *  Implementation of producer / consumer issue.
 *  Processes must be synchronized and data structures blocked-
 *  otherwise, could run out of array bounds
 * 
 */

import java.util.LinkedList;
import java.util.List;

class P {
    private static final int LIM = 10;
    private List<Integer> l = new LinkedList<>();
    private int i = 0;
    
    P() {
    }
    
    public void produce() throws InterruptedException {
        while(true) {
            
            synchronized(this) {
                
                while(l.size() == LIM) {
                    wait();
                }
                
                l.add(++i);
                notify();
            }
            
            System.out.println(l.size());
        }
    }
    
    public void consume() throws InterruptedException {
        while(true) {
            
            synchronized(this) {
                
                while(l.size() == 0) {
                    wait();
                }
                
                l.remove(0);
                notify();
            }

        }
    }
}


public class WaitNotify {
    public static void main(String[] args) {
        final P p = new P();
        
        Thread producer = new Thread(new Runnable() {
            
            @Override
            public void run() {
                try {
                    p.produce();
                } catch(InterruptedException e) { e.printStackTrace(); }
            }
            
        });

        Thread consumer = new Thread(new Runnable() {
            
            @Override
            public void run() {
                try {
                    p.consume();
                } catch(InterruptedException e) { e.printStackTrace(); }
            }
            
        });
        
        producer.start();
        consumer.start();
        
        try {
            producer.join();
            consumer.join();
        } catch(InterruptedException e) {
            e.printStackTrace();
        }
    }
}
