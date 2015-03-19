package spam;

public class Email implements Message {
	//TODO - add url links
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
				.replaceAll("[\\s]{2,}", " ")
				.trim();
	}
	
	public String getBody() {
		return body;
	}
	
	public String getSender() {
		return sender;
	}
	
	public String getSubject() {
		return subject;
	}
}
