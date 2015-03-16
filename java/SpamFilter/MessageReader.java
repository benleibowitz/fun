package spam;

public class MessageReader {
	private Message message;
	
	MessageReader() {
	}
	
	public void setMessage(Message message) {
		if(message == null)
			throw new IllegalArgumentException("Message cannot be null");
		
		this.message = message;
	}
	
	public boolean isSpam(Message message) {
		setMessage(message);
		return isSpam();
	}
	
	public boolean isSpam() {
		if(message == null)
			throw new IllegalArgumentException("Message cannot be null");
		
		ProbabilityCalculator probabilityCalc = ProbabilityCalculator.getInstance();
		return probabilityCalc.isSpam(message);
	}
}
