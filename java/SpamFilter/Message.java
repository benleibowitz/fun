package spam;

public class Message {
	//TODO - add url links, subject, sender
	private String body;
	
	public Message(String body) {
		if(body == null)
			throw new NullPointerException("Message body cannot be null");
		
		this.body = body;
	}
	
	public String getBody() {
		return body;
	}
	
	public String[] getBodyWords() {
		return body.replaceAll("[^\\w^\\s]", "").split(" ");
	}
}
