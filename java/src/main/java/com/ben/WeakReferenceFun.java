package com.ben;
import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Map;
import java.util.WeakHashMap;


public class WeakReferenceFun {
    private static class Node {
        private final int foo;
        public Node() {
            foo = 3;
        }
        public int getFoo() {
            return foo;
        }
        
    }
    public static void main(String[] args) {
        weakRefs();
        weakMap();
        weakRefs2();
    }

    public static void weakRefs2() {
        System.out.println("----------Weak References 2------------");
        
        Node n = new Node();
        WeakReference<Node> wr = new WeakReference(n);
        System.out.println(wr + ", " + wr.get());
        
        n= null;
        System.gc();
        System.out.println("\n ## Garbage collector running ## \n");
        System.out.println(wr + ", " + wr.get());

        System.out.println("---------------------------------");
    }
    
    public static void weakRefs() {
        System.out.println("----------Weak References-------------");
        
        WeakReference<Node> wr = makeRef();
        
        System.out.println(wr + ", " + wr.get());
        System.gc();
        System.out.println("\n ## Garbage collector running ## \n");
        System.out.println(wr + ", " + wr.get());

        System.out.println("---------------------------------");
    }
    
    public static WeakReference<Node> makeRef() {
        Node n = new Node();
        return new WeakReference<Node>(n);
    }
    
    public static void weakMap() {
        System.out.println("---------Weak HashMap------------");
        Map<String, String> m = new WeakHashMap<>();
        Map<String, String> regM = new HashMap<>();
        
        String weakKey = new String("weakKey");
        String strongKey = new String("strongKey");
        String v = "val";

        m.put(weakKey, v);
        regM.put(strongKey, v);

        System.out.println("weakMap: " + m);
        System.out.println("regularMap: " + regM);
        
        weakKey = null;
        strongKey = null;
        gc();
        
        System.out.println("weakMap: " + m);
        System.out.println("regularMap: " + regM);
        System.out.println("---------------------------------");
    }
    
    public static void gc() {
        System.gc();
        System.out.println("\n ## Garbage collector running ## \n");
    }
    
}
