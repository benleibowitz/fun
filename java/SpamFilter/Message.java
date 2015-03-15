package spam;

public class Message {
	//Maybe add url links, subject, sender as well
	private String body;
	
	public Message(String body) {
		this.body = body;
	}
	
	public String getBody() {
		return body;
	}
}
