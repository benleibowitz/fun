/*		
 * The calculation for this Bayes algorithm is:		
 * for each word, W, in an email message		
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
package email;
import java.util.Map;
import java.util.List;

public class BayesEmailAlgorithm implements SpamAlgorithm {
	private static final double BODY_WEIGHT = 0.35;
	private static final double SENDER_WEIGHT = 0.2;
	private static final double SUBJECT_WEIGHT = 0.45;
	
	private BayesEmailScoringSystem scoringSystem;
	
	public BayesEmailAlgorithm(BayesEmailScoringSystem scoringSystem) {
		if(scoringSystem == null)
			throw new IllegalArgumentException("Scoring system cannot be null");
			
		this.scoringSystem = scoringSystem;
	}
	
	@Override
	public boolean isSpam(Email email) {
		if(email == null)
			throw new IllegalArgumentException("Message cannot be null");
		
		double weightedProbability = BODY_WEIGHT * processText(email.getBody(), scoringSystem.getBodyProbabilityMap())
				+ SENDER_WEIGHT * processText(email.getSender(), scoringSystem.getSenderProbabilityMap())
				+ SUBJECT_WEIGHT * processText(email.getSubject(), scoringSystem.getSubjectProbabilityMap());
		
		System.out.println(weightedProbability);
		
		return (weightedProbability > 0.5);
	}
	
	private double processText(String text, Map<String, int[]> wordCountMap) {
		List<String> genericWords = scoringSystem.getGenericWords();
		double probabilitySpam = 0;
		double sumLogsSpam = 0;
		
		String[] bodyWords = text.split(" ");
		
		/*
		 * For each word in message body, check the both the current word and
		 * combo of (previous word + current word) for matches in the mapping.
		 * EXAMPLE: if word is "here" and previous word is "click",
		 * get P(S|"here") and P(S|"click here")
		 */
		for(int i = 0; i < bodyWords.length; i++) {
			String[] wordCombos = new String[]{bodyWords[i]};
			
			if(i > 0) {
				String adjacentWords = bodyWords[i-1] + " " + bodyWords[i];
				wordCombos = new String[]{bodyWords[i], adjacentWords};
			}
				
			for(String wordOrPhrase : wordCombos) {
				if(wordCountMap.containsKey(wordOrPhrase) && !(genericWords.contains(wordOrPhrase))) {
					//Calculate probability of spam / real
					int totalWords = wordCountMap.get(wordOrPhrase)[0] + wordCountMap.get(wordOrPhrase)[1];
					double probSpamWord = (double)wordCountMap.get(wordOrPhrase)[0] / totalWords;
					double probRealWord = (double)wordCountMap.get(wordOrPhrase)[1] / totalWords;
					
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
