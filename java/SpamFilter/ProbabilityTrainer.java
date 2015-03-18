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
	private static final String BODYMAP_FILE = "C:/Users/Ben/workspace/JavaProjects/src/spam/bodyMap.csv";
	private static final String SUBJECTMAP_FILE = "C:/Users/Ben/workspace/JavaProjects/src/spam/subjectMap.csv";
	private static final String SENDERMAP_FILE = "C:/Users/Ben/workspace/JavaProjects/src/spam/senderMap.csv";
	
	//All words in map are lowercase.
	//Probability map contains: <word, { P(word is in spam message), P(word is in real message) }>
	private Map<String, ArrayList<Double>> bodyProbabilityMap;
	private Map<String, ArrayList<Double>> subjectProbabilityMap;
	private Map<String, ArrayList<Double>> senderProbabilityMap;
	
	private Map<String, Map<String, ArrayList<Double>>> fileMap = new HashMap<>();
	
	ProbabilityTrainer() {
		initialize();
	}
	
	public void initialize() {
		bodyProbabilityMap = new HashMap<>();
		senderProbabilityMap = new HashMap<>();
		subjectProbabilityMap = new HashMap<>();
		
		fileMap.put(BODYMAP_FILE, bodyProbabilityMap);
		fileMap.put(SENDERMAP_FILE, senderProbabilityMap);
		fileMap.put(SUBJECTMAP_FILE, subjectProbabilityMap);
		
		readMapping();
	}
	
	private void readMapping() {

		for(String fileName : fileMap.keySet()) {
			Map<String, ArrayList<Double>> probabilityMap = fileMap.get(fileName);
			
			//TODO - use OPENCSV
			BufferedReader bufferedReader = null;
			
			try {
				bufferedReader = new BufferedReader(new FileReader(new File(fileName)));
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
	}
	
	public void commit() {
		
		for(String fileName : fileMap.keySet()) {
			Map<String, ArrayList<Double>> probabilityMap = fileMap.get(fileName);
			
			//Write new word map to file
			BufferedWriter bufferedWriter = null;
			
			try {
				bufferedWriter = new BufferedWriter(new FileWriter(
						new File(fileName)));
				
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
		
	}
	
	public void train(Message message, boolean spam) {
		train(message.getBody(), spam, bodyProbabilityMap);
		train(message.getSender(), spam, senderProbabilityMap);
		train(message.getSubject(), spam, subjectProbabilityMap);
	}
	
	private void train(String text, boolean spam, Map<String, ArrayList<Double>> probabilityMap) {
		String[] words = text.split(" ");
		for(int i = 0; i < words.length; i++) {
			
			//Setup String[] with current word, and combo of (current word + previous word)
			String[] wordOrPhrase = new String[]{ words[i] };
			
			if(i > 0)
				wordOrPhrase = new String[]{ words[i], words[i] + " " + words[i - 1]};
				
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