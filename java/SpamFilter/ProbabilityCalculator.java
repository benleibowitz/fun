package spam;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class ProbabilityCalculator {
	private SpamAlgorithm algorithm;
	
	public ProbabilityCalculator(SpamAlgorithm algorithm) {
		if(algorithm == null)
			throw new IllegalArgumentException("ProbabilityCalculator arguments cannot be null");
		
		this.algorithm = algorithm;
		initialize();
	}
	
	private void initialize() {
		//do any remaining initialization work here
	}
	
	public boolean isSpam(Message message) {
		return algorithm.isSpam(message);
	}
}
