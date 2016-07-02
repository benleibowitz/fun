/*
 * Testing memcached with spymemcached client,
 * using Google Snappy to compress the objects
 * before writing to cache.
 */
package com.ben.frameworktesting;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import net.spy.memcached.AddrUtil;
import net.spy.memcached.MemcachedClient;

import org.xerial.snappy.Snappy;

import com.ben.datastructures.Node;

public class MemcachedTest {
    public static void main(String[] args) {
        
        MemcachedClient cache = null;
        
        try {
            //get the memcached client
            cache = new MemcachedClient(AddrUtil.getAddresses("127.0.0.1:11211"));

            //if myobj already in cache, uncompress it and print it. else print 'not in cache'
            System.out.println((cache.get("myobj") == null) ? ("myobj not in cache") :
                        ("myobj found in cache:" + cache.get("myobj") + " -> data=" + readAndUncompress(cache).getData()));
            
            //Create new node, compress it, and write it to cache
            Node n = Node.builder()
                    .data(14)
                    .build();
            System.out.println("Writing to cache: " + n.getClass().getName() + "(" + n.getData() + ")");
            compressAndCache(cache, n);
            
            //read new node from cache and uncompress it
            System.out.println("Read from cache: " + readAndUncompress(cache).getClass().getName() + "(" + n.getData() + ")");
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if(cache != null) {
                System.out.println("Shutting down");
                cache.shutdown();
            }
        }
        
    }
    
    public static void compressAndCache(MemcachedClient cache, Node n) throws IOException {
        //compress object to byte array
        ByteArrayOutputStream b = new ByteArrayOutputStream();
        ObjectOutputStream o = new ObjectOutputStream(b);
        o.writeObject(n);
        byte[] compressed = Snappy.compress(b.toByteArray());
        
        cache.set("myobj", 100, compressed);
        
        o.close();
        b.close();
    }
    
    public static Node readAndUncompress(MemcachedClient cache) throws IOException, ClassNotFoundException {
        byte[] bytes = (byte[])cache.get("myobj");
        byte[] uncompressed = Snappy.uncompress(bytes);
        
        ByteArrayInputStream b = new ByteArrayInputStream(uncompressed);
        ObjectInputStream o = new ObjectInputStream(b);
        Node<Integer> n = (Node) o.readObject();
        
        o.close();
        b.close();
        
        return n;
    }
}
