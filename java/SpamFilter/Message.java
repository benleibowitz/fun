package spam;

public class Message {
	//TODO - add url links, subject, sender
	private String sender;
	private String subject;
	private String body;
	
	public Message(String sender, String subject, String body) {
		if(body == null || sender == null || subject == null)
			throw new IllegalArgumentException("Message arguments cannot be null");
		
		this.body = processText(body);
		this.subject = processText(subject);
		this.sender = processText(sender);
	}
	
	private String processText(String text) {
		return text.toLowerCase()
				.replace("-", " ")
				.replaceAll("<br>|[^\\w^\\s]|_", "")
				.replaceAll("[\\s]{2,}", " ");
	}
	
	public String getBody() {
		return body;
	}
	
	public String[] getBodyWords() {
		return body.split(" ");
	}
	
	public String getSender() {
		return sender;
	}
	
	public String getSubject() {
		return subject;
	}
}
