package spam;

public class Message {
	//TODO - add url links, subject, sender
	private String body;
	
	public Message(String body) {
		if(body == null)
			throw new IllegalArgumentException("Message body cannot be null");
		
		this.body = body.replaceAll("[^\\w^\\s]|_", "").toLowerCase();
	}
	
	public String getBody() {
		return body;
	}
	
	public String[] getBodyWords() {
		return body.split(" ");
	}
}
