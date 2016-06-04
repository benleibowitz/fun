package com.ben.database;

/**
 * This class connects to a MongoDB instance running
 * on localhost, port 27017. It retrieves all records 
 * from the "zips" collection of the "geo" database
 * where population > 50000.
 * 
 * This class assumes you have loaded your MongoDB instance
 * with the sample data found here:
 *        http://media.mongodb.org/zips.json
 *  
 */
import org.bson.Document;

import com.mongodb.BasicDBObject;
import com.mongodb.MongoClient;
import com.mongodb.MongoClientURI;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;
import com.mongodb.client.MongoDatabase;

public class MongoExample {
    public static void main(String[] args) {
        //Get MongoClient and connect to DB
        MongoClientURI uri = new MongoClientURI("mongodb://localhost:27017/geo"); 
        MongoClient client = null;
        
        try{
            //Get the MongoClient and MongoDatabase
            client = new MongoClient(uri);
            MongoDatabase db = client.getDatabase(uri.getDatabase());
            
            //Get MongoDB Collection
            MongoCollection<Document> collection = db.getCollection("zips");
            
            //Create search query (select where population > 50000)
            BasicDBObject searchQuery = new BasicDBObject();
            BasicDBObject queryGreaterThan = new BasicDBObject();
            queryGreaterThan.put("$gt", 50000);
            searchQuery.put("pop", queryGreaterThan);
           
            System.out.println("Results returned:");
            System.out.println("-----------------");
            
            try (MongoCursor<Document> cursor = collection.find(searchQuery).iterator()) {
                while (cursor.hasNext()) {

                    Document doc = cursor.next();
                    String city = doc.get("city").toString();
                    String state = doc.get("state").toString();
                    String zip = doc.get("_id").toString();
                    int pop = Double.valueOf(doc.get("pop").toString()).intValue();
                    
                    System.out.println(zip + "- " + city + ", " + state + "= " + pop);
                }
            }
            
        } finally {
            if(client != null) {
                client.close();
            }
        }
    }
}
