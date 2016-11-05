package com.ben.frameworktesting;
/*
 * Testing Google's Snappy compression algorithm
 * by compressing a test CSV file.
 */
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.xerial.snappy.Snappy;

public class SnappyTest {
    public static void main(String[] args) {
        
        FileOutputStream oStream = null;
        
        try {
            
            byte[] fileBytes = Files.readAllBytes(Paths.get("/Users/ben/Documents/Code/CSV/testcsv.csv"));
            String fileString = new String(fileBytes, StandardCharsets.UTF_8);
            
            byte[] compressedBytes = Snappy.compress(fileString);
            
            oStream = new FileOutputStream(new File("/Users/ben/Documents/Code/CSV/testcsv_encoded.txt"));
            oStream.write(compressedBytes);

            System.out.println("Compressed file written");

            byte[] compressedBytesFromFile = Files.readAllBytes(Paths.get("/Users/ben/Documents/Code/CSV/testcsv_encoded.txt"));
            byte[] uncompressedBytesFromFile = Snappy.uncompress(compressedBytesFromFile);
            String uncompressedString = new String(uncompressedBytesFromFile, StandardCharsets.UTF_8);

            System.out.println("Uncompressed");
            System.out.println(uncompressedString);
        
        } catch(Exception e) {
            e.printStackTrace();
        } finally {
            if(oStream != null) {
                
                try {
                    oStream.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
                
            }
        }
        
    }
}
