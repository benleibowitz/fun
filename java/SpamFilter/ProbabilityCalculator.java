package spam;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;

public class ProbabilityCalculator {
	//all words in map must be lowercase
	//probability map contains <word, { p(word is in spam message), p(word is in real message) }>
	private Map<String, double[]> probabilityMap;
	private static final String wordMapFile = "C:/Users/Ben/workspace/JavaProjects/src/spam/WordMap.csv";
	private static ProbabilityCalculator _instance;
	
	private ProbabilityCalculator() {
	}
	
	private void initialize() {
		probabilityMap = new HashMap<>();
		
		//TEST read csv mapping
		BufferedReader br = null;
		try {
			br = new BufferedReader(new FileReader(wordMapFile));
			String line;
			br.readLine();
			
			while((line = br.readLine()) != null) {
				String[] ar = line.split(",");
				double totMessages = Double.valueOf(ar[1]);
				double spamMessages = Double.valueOf(ar[2]);
				double realMessages = Double.valueOf(ar[3]);
				probabilityMap.put(ar[0], new double[]{spamMessages/totMessages, realMessages/totMessages});
			}
		} catch(Exception e) {
			e.printStackTrace();
		}
	}
	
	public static ProbabilityCalculator getInstance() {
		if(_instance == null) {
			_instance = new ProbabilityCalculator();
			_instance.initialize();
		}
		
		return _instance;
	}
	
	public boolean isSpam(Message message) {
		double probabilitySpam = 1;
		double probabilityReal = 1;
		
		String[] bodyWords = message.getBodyWords();
		for(int i = 0; i < bodyWords.length; i++) {
			//check individual word
			String word = bodyWords[i];

			if(probabilityMap.containsKey(word)) {
				probabilitySpam *= probabilityMap.get(word)[0];
				probabilityReal *= probabilityMap.get(word)[1];
			}
			
			if(i < (bodyWords.length - 1)) {
				//check combination of 2 words
				String comboWord = word + " " + bodyWords[i+1];
				
				if(probabilityMap.containsKey(comboWord)) {
					probabilitySpam *= probabilityMap.get(comboWord)[0];
					probabilityReal *= probabilityMap.get(comboWord)[1];
				}
			}
		}
		
		System.out.println("Spam:" + probabilitySpam + " Real:" + probabilityReal);
		
		if(probabilitySpam > probabilityReal)
			return true;
		else
			return false;
	}
}
