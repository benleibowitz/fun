/*
 * Testing Redis with Jedis client
 */
package com.ben;

import redis.clients.jedis.Jedis;

public class RedisTest {
    public static void main(String[] args) {
        
        Jedis jedis = null;
        
        try {
            //get the Jedis client
            jedis = new Jedis("localhost");
            System.out.println(jedis.ping());
            
            jedis.set("myKey", "myVal");
            System.out.println(jedis.get("myKey"));
            
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if(jedis != null) {
                jedis.close();
            }
        }
        
    }
}
