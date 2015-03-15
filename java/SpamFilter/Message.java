package spam;

public class Message {
	//Maybe add url links, subject, sender as well
	private String body;
	
	public Message(String body) {
		if(body == null)
			throw new IllegalArgumentException("Body cannot be null");
		
		this.body = body;
	}
	
	public String getBody() {
		return body;
	}
}
