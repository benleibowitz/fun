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

public class BayesProbabilityTrainer implements ProbabilityTrainer {
	private BayesScoringSystem scoringSystem;
	
	ProbabilityTrainer(BayesScoringSystem scoringSystem) {
		if(scoringSystem == null)
			throw new IllegalArgumentException("Scoring system cannot be null");
			
		this.scoringSystem = scoringSystem;
		initialize();
	}
	
	public void initialize() {
		//Do any initialization work here
	}
	
	@Override
	public void commit() {
		probabilityMap.write();
	}
	
	@Override
	public void train(Message message, boolean spam) {
		if(message == null || spam == null)
			throw new IllegalArgumentException("Arguments cannot be null for train");
		
		train(message.getBody(), spam, scoringSystem.getBodyProbabilityMap());
		train(message.getSender(), spam, scoringSystem.getSenderProbabilityMap());
		train(message.getSubject(), spam, scoringSystem.getSubjectProbabilityMap());
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
