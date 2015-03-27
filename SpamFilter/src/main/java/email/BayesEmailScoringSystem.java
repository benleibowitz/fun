/*
 * BayesScoringSystem is an object containing probability maps for
 * sender, subject, and message body. Each probability map is in 
 * form:
 * 	Map<String wordOrPhrase, int[]{probSpamMessage, probRealMessage} >
 * WHERE:
 *	probSpamMessage = (# spam messages containing word) / (# total messages containing word)
 *	probRealMessage = (# real messages containing word) / (# total messages containing word)
 *
 */
package email;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class BayesEmailScoringSystem {
	private static final String BASE_URL = "src/main/resources/";
	private static final String BODYMAP_FILE = BASE_URL + "bodyMap.csv";
	private static final String SUBJECTMAP_FILE = BASE_URL + "subjectMap.csv";
	private static final String SENDERMAP_FILE = BASE_URL + "senderMap.csv";
	private static final String GENERICWORD_FILE = BASE_URL + "genericWords.csv";
	
	//All words in map are lowercase.
	//Probability map contains: <word, { P(word is in spam message), P(word is in real message) }>
	private Map<String, int[]> bodyProbabilityMap;
	private Map<String, int[]> subjectProbabilityMap;
	private Map<String, int[]> senderProbabilityMap;
	
	//Contains <MappingFileURLString, respectiveProbabilityMap>
	private Map<String, Map<String, int[]>> fileMap;
	
	//Contains words like "if" "and" "the" "I"
	private List<String> genericWords;
	
	public BayesEmailScoringSystem() {
		initialize();
	}
	
	private void initialize() {
		genericWords = new ArrayList<>();
		readGenericWords();
		
		fileMap = new HashMap<>();
    	
		bodyProbabilityMap = new HashMap<>();
		senderProbabilityMap = new HashMap<>();
		subjectProbabilityMap = new HashMap<>();
		
		fileMap.put(BODYMAP_FILE, bodyProbabilityMap);
		fileMap.put(SENDERMAP_FILE, senderProbabilityMap);
		fileMap.put(SUBJECTMAP_FILE, subjectProbabilityMap);
		
		//TEST read CSV word file
		//TODO - implement CSV reader class
		for(String fileName : fileMap.keySet()) {
			Map<String, int[]> wordCountMap = fileMap.get(fileName);
			
			BufferedReader br = null;
			try {
				br = new BufferedReader(new FileReader(fileName));
				String line;
				br.readLine();
				
				while((line = br.readLine()) != null) {
					String[] ar = line.split(",");
					int spamMessages = Integer.valueOf(ar[1]);
					int realMessages = Integer.valueOf(ar[2]);
					wordCountMap.put(ar[0], new int[]{spamMessages, realMessages});
				}
				
			} catch(FileNotFoundException e) {
				e.printStackTrace();
			} catch(IOException e) {
				e.printStackTrace();
			} finally {
				if(br != null) {
					try {
						br.close();
					} catch(IOException e) {
						e.printStackTrace();
					}
				}
			}
		}
	}
	
	public void readGenericWords() {
		BufferedReader bufferedReader = null;
		
		try {
			bufferedReader = new BufferedReader(new FileReader(GENERICWORD_FILE));
			
			String line;
			//Read headers
			bufferedReader.readLine();
			
			while((line = bufferedReader.readLine()) != null) {
				genericWords.add(line.replace("\n", ""));
			}
		} catch(FileNotFoundException e) {
			System.out.println("Could not read generic words file: " + GENERICWORD_FILE);
			e.printStackTrace();
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
	
	public void write() {
	    
		for(String fileName : fileMap.keySet()) {
			//Write new word map to file
			BufferedWriter bufferedWriter = null;
			Map<String, int[]> wordCountMap = fileMap.get(fileName);
                    
			try {
				bufferedWriter = new BufferedWriter(new FileWriter(
						new File(fileName)));
                
				//Write headers
				bufferedWriter.write("Word,SpamMessages,RealMessages\n");
                
				for(String word : wordCountMap.keySet()) {
                    
					bufferedWriter.write(word + "," + wordCountMap.get(word)[0]
							+ "," + wordCountMap.get(word)[1] + "\n" );
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
  
	public void setBodyProbabilityMap(Map<String, int[]> bodyProbabilityMap) {
		this.bodyProbabilityMap = bodyProbabilityMap;
	}
  
	public void setSenderProbabilityMap(Map<String, int[]> senderProbabilityMap) {
		this.senderProbabilityMap = senderProbabilityMap;
	}
  
	public void setSubjectProbabilityMap(Map<String, int[]> subjectProbabilityMap) {
		this.subjectProbabilityMap = subjectProbabilityMap;
	}
  
	public Map<String, int[]> getBodyProbabilityMap() {
		return bodyProbabilityMap;
	}
  
	public Map<String, int[]> getSenderProbabilityMap() {
		return senderProbabilityMap;
	}
  
	public Map<String, int[]> getSubjectProbabilityMap() {
		return subjectProbabilityMap;
	}
	
	public List<String> getGenericWords() {
		return genericWords;
	}
  
}
