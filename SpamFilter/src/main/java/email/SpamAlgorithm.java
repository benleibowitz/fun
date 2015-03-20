package email;

public interface SpamAlgorithm {
	
	/*
	 * If a word's probability of being a spam word is more than LEGITIMATE_WORD_THRESHOLD
	 * away from 0.5, (ie, how far it is from 50/50), use the word in calculation.
	 * This helps us avoid words that are 50%/50% spam/real, and Bayesian poisoning
	 * EXAMPLE: if LEGITIMATE_WORD_THRESHOLD=0.2, word will be
	 * used if probability spam of a given word is < .3 or > .7
	*/
	static final double LEGITIMATE_WORD_THRESHOLD = 0.35;
	
	//The probability that any message is spam
	//(statistics say it's anywhere from 0.5 to 0.8)
	static final double PROBABILITY_SPAM_MESSAGE = 0.5;
	
	public boolean isSpam(Email email);
}
