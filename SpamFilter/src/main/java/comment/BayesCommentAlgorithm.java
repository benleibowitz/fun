/*		
 * The calculation for this Bayes algorithm is:		
 * for each word, W, in an comment message		
 * the probability P(S|W) that given W, the message is spam		
 * is:		
 * 	P(S|W) = P(W|S) * P(S) / ( P(W|S) * P(S) + P(W|R) * P(R) )		
 *		 WHERE:		
 *		 P(W|S) -> probability that word W appears in spam messages		
 *		 P(S)   -> probability that any message is spam		
 *		 P(W|R) -> probability that word W appears in real messages		
 *		 P(R)   -> probability that any message is real (1 - P(S))		
 *		
 * The combined probability P that an entire message is spam is calculated as:		
 * 	P = 1 / (1 + e^n)		
 * 		WHERE:		
 *		n	-> SUM(1->N){ ln(1 - P(S|Wi)) - ln( P(S|Wi) ) }		
 * 		P(S|Wi)	-> P(S|W) for each word Wi
 *		
 */
package comment;
import java.util.Map;

public class BayesCommentAlgorithm implements SpamAlgorithm {
	private static final double LEGITIMATE_WORD_THRESHOLD = 0.35;
	private static final double PROBABILITY_SPAM_MESSAGE = 0.3;
	
	private BayesCommentScoringSystem scoringSystem;
	
	public BayesCommentAlgorithm(BayesCommentScoringSystem scoringSystem) {
		if(scoringSystem == null)
			throw new IllegalArgumentException("Scoring system cannot be null");
			
		this.scoringSystem = scoringSystem;
	}
	
	@Override
	public boolean isSpam(Comment comment) {
		if(comment == null)
			throw new IllegalArgumentException("Message cannot be null");
		
		double weightedProbability = processText(comment.getBody(), scoringSystem.getBodyProbabilityMap());
		
		System.out.println(weightedProbability);
		if(weightedProbability > 0.5)
			return true;
		else
			return false;
	}
	
	private double processText(String text, Map<String, double[]> probabilityMap) {
		double probabilitySpam = 0;
		double sumLogsSpam = 0;
		
		String[] bodyWords = text.split(" ");
		
		/*
		 * For each word in comment body, check the word and
		 * combo of (previous word + current word) for matches in the mapping.
		 */
		for(int i = 0; i < bodyWords.length; i++) {
			String[] wordCombos = new String[]{bodyWords[i]};
			
			if(i > 0) {
				String adjacentWords = bodyWords[i-1] + " " + bodyWords[i];
				wordCombos = new String[]{bodyWords[i], adjacentWords};
			}
				
			for(String wordOrPhrase : wordCombos) {
				
				if(probabilityMap.containsKey(wordOrPhrase)) {
					//Calculate probability of spam / real
					double probSpamWord = probabilityMap.get(wordOrPhrase)[0];
					double probRealWord = probabilityMap.get(wordOrPhrase)[1];
					
					//Check threshold and add to total probability
					if(Math.abs(0.5 - probSpamWord) > LEGITIMATE_WORD_THRESHOLD) {
						
						//Don't want 0 numerator, as Math.log(0) returns negative infinity.
						if(probSpamWord == 0)
							probSpamWord = 0.05;
						if(probRealWord == 0)
							probRealWord = 0.05;
						
						double pSpamNumerator = probSpamWord * PROBABILITY_SPAM_MESSAGE;
						double pDenom = (probSpamWord * PROBABILITY_SPAM_MESSAGE)
								+ (probRealWord * (1 - PROBABILITY_SPAM_MESSAGE));
						
						sumLogsSpam +=
								(Math.log(1 - pSpamNumerator/pDenom) - Math.log(pSpamNumerator/pDenom));
					}
				}
			}
		}
		
		probabilitySpam = 1 / (1 + Math.pow(Math.E, sumLogsSpam));
		
		return probabilitySpam;
	}
}
