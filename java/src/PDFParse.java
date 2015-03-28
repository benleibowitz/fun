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

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.util.PDFTextStripper;
import org.apache.pdfbox.io.RandomAccess;
import org.apache.pdfbox.io.RandomAccessFile;
import org.apache.tika.io.TemporaryResources;

import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;

public class PDFParse {
    static final Logger log = Logger.getLogger(PDFParse.class);

    public static void main(String[] args) {
        
        if(args.length != 1) {
            System.out.println("Usage: ConvertPDF [PDF File]");
            System.exit(1);
        }

	PropertyConfigurator.configure("/home/ben/Documents/Code/JavaJars/apache-log4j-1.2.17/buildfile.writetofile");   
	PDDocument doc = null;
        
        try {
            doc = readInputFile(args[0]);
                        
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
    
    public static PDDocument readInputFile(String fileURL) {
        if(fileURL == null || fileURL.length() < 1)
            throw new IllegalArgumentException("File URL cannot be null or empty");
	
	TemporaryResources tmp = new TemporaryResources();
	RandomAccess scratchFile = new RandomAccessFile(tmp.createTemporaryFile(), "rw");
	
	File inputFile = new File(fileURL);
	return PDDocument.loadNonSeq(inputFile, scratchFile);
    }
    
}
