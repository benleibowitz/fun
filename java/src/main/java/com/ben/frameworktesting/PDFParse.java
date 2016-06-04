package com.ben.frameworktesting;
/*
 *
 *  PDFParse.java takes a PDF file as input and
 *  extracts the text, and prints to console.
 *  (Does not use optical character recognition..
 *  Uses Apache PDFBox to strip text elements of 
 *  text-based PDF)
 *
 */

import java.io.File;
import java.io.IOException;

import org.apache.log4j.Logger;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.text.PDFTextStripper;

public class PDFParse {
    static final Logger log = Logger.getLogger(PDFParse.class);

    public static void main(String[] args) {
        
        if(args.length != 1) {
            System.out.println("Usage: ConvertPDF [PDF File]");
            System.exit(1);
        }
        
        String filePath = args[0];

        PDDocument doc = null;
        
        try {
            doc = readInputFile(filePath);
                        
            PDFTextStripper textStrip = new PDFTextStripper();
            textStrip.setStartPage(1);
            textStrip.setEndPage(doc.getNumberOfPages());
            
            System.out.println(textStrip.getText(doc));
            
        } catch(IOException e) {
            e.printStackTrace();
        } finally {

            if(doc != null) {
                try {
                    doc.close();
                } catch(Exception e) {
                    e.printStackTrace();
                }
            }
        }
    
    }
    
    public static PDDocument readInputFile(String fileURL) throws IOException {
        if(fileURL == null || fileURL.length() < 1)
            throw new IllegalArgumentException("File URL cannot be null or empty");
	
        File inputFile = new File(fileURL);
        return PDDocument.load(inputFile);
    }
    
}
