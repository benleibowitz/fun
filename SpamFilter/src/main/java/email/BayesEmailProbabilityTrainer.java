package email;

import java.util.Map;
import java.util.List;

public class BayesEmailProbabilityTrainer implements ProbabilityTrainer {
	private BayesEmailScoringSystem scoringSystem;
	
	public BayesEmailProbabilityTrainer(BayesEmailScoringSystem scoringSystem) {
		if(scoringSystem == null)
			throw new IllegalArgumentException("Scoring system cannot be null");
			
		this.scoringSystem = scoringSystem;
	}
	
	@Override
	public void commit() {
		scoringSystem.write();
	}
	
	@Override
	public void train(Email email, boolean spam) {
		if(email == null)
			throw new IllegalArgumentException("Message cannot be null");
		
		train(email.getBody(), spam, scoringSystem.getBodyProbabilityMap());
		train(email.getSender(), spam, scoringSystem.getSenderProbabilityMap());
		train(email.getSubject(), spam, scoringSystem.getSubjectProbabilityMap());
		train(email.getLinks(), spam, scoringSystem.getLinkProbabilityMap());
	}
	
	private void train(String text, boolean spam, Map<String, double[]> probabilityMap) {
		List<String> genericWords = scoringSystem.getGenericWords();
		
		String[] words = text.split(" ");
		for(int i = 0; i < words.length; i++) {
			
			//Setup String[] with current word, and combo of (current word + previous word)
			String[] wordOrPhrase;
			
			if(i == 0)
				wordOrPhrase = new String[]{ words[i] };
			else
				wordOrPhrase = new String[]{ words[i], words[i-1] + " " + words[i]};
				
			for(String word : wordOrPhrase) {
				double[] probs;
				
				if(!(genericWords.contains(word))) {
					
					if(probabilityMap.containsKey(word)) {
						//If word found in map, increment
						probs = probabilityMap.get(word);
					} else {
						//Word is not in map. Add it
						probs = new double[]{0,0};					
					}
					if(spam) 
						probs[0] += 1;
					else
						probs[1] += 1;
					
					probabilityMap.put(word, probs);
				}
			}
		}
	}
}

