package spam;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class ProbabilityTrainer {
	private static final String WORDBANK_FILE = "C:/Users/Ben/workspace/JavaProjects/src/spam/WordMap.csv";
	
	//Contains <word, ArrayList<totalwordcount, totalspamwords, totalrealwords> >
	private Map<String, ArrayList<Double>> probabilityMap;
	
	ProbabilityTrainer() {
		probabilityMap = new HashMap<>();
		initialize();
	}
	
	public void initialize() {
		readMapping();
	}
	
	public void readMapping() {

		//TODO - use OPENCSV
		BufferedReader bufferedReader = null;
		
		try {
			bufferedReader = new BufferedReader(new FileReader(new File(WORDBANK_FILE)));
			bufferedReader.readLine();
			String line;
			
			while((line = bufferedReader.readLine()) != null) {
				String[] words = line.toLowerCase().split(",");
				
				ArrayList<Double> l = new ArrayList<>();
				l.add(Double.valueOf(words[1]));
				l.add(Double.valueOf(words[2]));
				l.add(Double.valueOf(words[3]));
				
				probabilityMap.put(words[0], l);
			}
		} catch(IOException e) {
			e.printStackTrace();
		} finally {
			if(bufferedReader != null) {
				try {
					bufferedReader.close();
				} catch(IOException e) {
					e.printStackTrace();
				}
			}
		}
	}
	
	public void commit() {
		//Write new word map to file
		BufferedWriter bufferedWriter = null;
		
		try {
			bufferedWriter = new BufferedWriter(new FileWriter(
					new File("C:/Users/Ben/workspace/JavaProjects/src/spam/WordMap.csv")));
			
			//Write headers
			bufferedWriter.write("Word,MessagesFoundIn,SpamMessages,RealMessages\n");
			
			for(String w : probabilityMap.keySet()) {
				
				bufferedWriter.write(w + "," + probabilityMap.get(w).get(0) + "," +
						probabilityMap.get(w).get(1) + "," + probabilityMap.get(w).get(2)
						+ "\n" );
			}
			
		} catch(IOException e) {
			e.printStackTrace();
		}  finally {
			if(bufferedWriter != null) {
				try {
					bufferedWriter.close();
				} catch(IOException e) {
					e.printStackTrace();
				}
			}
		}
		
	}
	
	public void train(Message message, boolean spam) {
		String[] bodyWords = message.getBodyWords();
		
		for(int i = 0; i < bodyWords.length; i++) {
			
			//Setup String[] with current word, and combo of (current word + previous word)
			String[] wordOrPhrase = new String[]{ bodyWords[i] };
			
			if(i > 0)
				wordOrPhrase = new String[]{ bodyWords[i], bodyWords[i] + " " + bodyWords[i - 1]};
				
			for(String word : wordOrPhrase) {
				ArrayList<Double> probs;
				
				if(probabilityMap.containsKey(word)) {
					//If word found in map, increment
					probs = probabilityMap.get(word);
					probs.set(0, probs.get(0) + 1.0);
					
					if(spam) 
						probs.set(1, probs.get(1) + 1.0);
					else
						probs.set(2, probs.get(2) + 1.0);
				
				} else {
					//Word is not in map. Add it
					probs = new ArrayList<>();
					probs.add(1.0);
					
					if(spam) {
						probs.add(1.0);
						probs.add(0.0);
					} else {
						probs.add(0.0);
						probs.add(1.0);
					}
					
					probabilityMap.put(word, probs);
				}
			}
		}
	}
}