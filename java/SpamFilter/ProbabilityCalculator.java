package spam;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class ProbabilityCalculator {
	private SpamAlgorithm algorithm;
	private ProbabilityMap probabilityMap;
	
	public ProbabilityCalculator(SpamAlgorithm algorithm, ProbabilityMap, probabilityMap) {
		if(algorithm == null || probabilityMap == null)
			throw new IllegalArgumentException("ProbabilityCalculator arguments cannot be null");
		
		this.algorithm = algorithm;
		this.probabilityMap = probabilityMap;
		initialize();
	}
	
	private void initialize() {
	}
	
	public boolean isSpam(Message message) {
		return algorithm.isSpam(message, probabilityMap);
	}
}
