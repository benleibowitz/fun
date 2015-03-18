package spam;

public class BayesScoringSystem implements ScoringSystem {
	private static final String BODYMAP_FILE = "C:/Users/Ben/workspace/JavaProjects/src/spam/bodyMap.csv";
	private static final String SUBJECTMAP_FILE = "C:/Users/Ben/workspace/JavaProjects/src/spam/subjectMap.csv";
	private static final String SENDERMAP_FILE = "C:/Users/Ben/workspace/JavaProjects/src/spam/senderMap.csv";
	
	//All words in map are lowercase.
	//Probability map contains: <word, { P(word is in spam message), P(word is in real message) }>
	private Map<String, double[]> bodyProbabilityMap;
	private Map<String, double[]> subjectProbabilityMap;
	private Map<String, double[]> senderProbabilityMap;
	
	private Map<String, Map<String, double[]>> fileMap = new HashMap<>();
	
	public BayesProbabilityMap() {
		initialize();
	}
	
	private void initialize() {
    		bodyProbabilityMap = new HashMap<>();
		senderProbabilityMap = new HashMap<>();
		subjectProbabilityMap = new HashMap<>();
		
		fileMap.put(BODYMAP_FILE, bodyProbabilityMap);
		fileMap.put(SENDERMAP_FILE, senderProbabilityMap);
		fileMap.put(SUBJECTMAP_FILE, subjectProbabilityMap);
		
		//TEST read CSV word file
		//TODO - implement CSV reader class
		for(String fileName : fileMap.keySet()) {
			Map<String, double[]> probabilityMap = fileMap.get(fileName);
			
			BufferedReader br = null;
			try {
				br = new BufferedReader(new FileReader(fileName));
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
  
  public void setBodyProbabilityMap(Map<String, double[]> bodyProbabilityMap) {
    this.bodyProbabilityMap = bodyProbabilityMap;
  }
  
  public void setSenderProbabilityMap(Map<String, double[]> senderProbabilityMap) {
    this.senderProbabilityMap = senderProbabilityMap;
  }
  
  public void setSubjectProbabilityMap(Map<String, double[]> subjectProbabilityMap) {
    this.subjectProbabilityMap = subjectProbabilityMap;
  }
  
  public Map<String, double[]> getBodyProbabilityMap() {
    return bodyProbabilityMap;
  }
  
  public Map<String, double[]> getSenderProbabilityMap() {
    return senderProbabilityMap;
  }
  
  public Map<String, double[]> getSubjectProbabilityMap() {
    return subjectProbabilityMap;
  }
  
  
}
