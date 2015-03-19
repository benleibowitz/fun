package spam;

import java.util.Map;

public class BayesProbabilityTrainer implements ProbabilityTrainer {
	private BayesScoringSystem scoringSystem;
	
	public BayesProbabilityTrainer(BayesScoringSystem scoringSystem) {
		if(scoringSystem == null)
			throw new IllegalArgumentException("Scoring system cannot be null");
			
		this.scoringSystem = scoringSystem;
	}
	
	@Override
	public void commit() {
		scoringSystem.write();
	}
	
	@Override
	public void train(Message message, boolean spam) {
		if(message == null)
			throw new IllegalArgumentException("Message cannot be null");
		
		train(message.getBody(), spam, scoringSystem.getBodyProbabilityMap());
		train(message.getSender(), spam, scoringSystem.getSenderProbabilityMap());
		train(message.getSubject(), spam, scoringSystem.getSubjectProbabilityMap());
	}
	
	private void train(String text, boolean spam, Map<String, double[]> probabilityMap) {
		String[] words = text.split(" ");
		for(int i = 0; i < words.length; i++) {
			
			//Setup String[] with current word, and combo of (current word + previous word)
			String[] wordOrPhrase = new String[]{ words[i] };
			
			if(i > 0)
				wordOrPhrase = new String[]{ words[i], words[i] + " " + words[i - 1]};
				
			for(String word : wordOrPhrase) {
				double[] probs;
				
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

