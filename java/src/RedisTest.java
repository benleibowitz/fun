/*
 * Testing Redis with Jedis client
 */
package redis;

import java.io.Serializable;
import java.util.List;

import redis.clients.jedis.Jedis;

class Node implements Serializable {

    /**
     * 
     */
    private static final long serialVersionUID = -2791375802729648470L;
    private int data;
    
    public Node(int d) {
        this.data = d;
    }
    
    public int getData() {
        return data;
    }
    public void setData(int data) {
        this.data = data;
    }
    
}

public class RedisTest {
    public static void main(String[] args) {
        
        Jedis jedis = null;
        
        try {
            //get the Jedis client
            jedis = new Jedis("localhost");
            System.out.println(jedis.ping());
            
            jedis.set("merp", "berp");
            System.out.println(jedis.get("merp"));
            
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if(jedis != null) {
                jedis.close();
            }
        }
        
    }
}
