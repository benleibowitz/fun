package spam;

public class Message {
	//TODO - add url links, subject, sender
	private String body;
	
	public Message(String body) {
		if(body == null)
			throw new IllegalArgumentException("Message body cannot be null");
		
		this.body = body.toLowerCase()
						.replace("-", " ")
						.replaceAll("<br>|[^\\w^\\s]|_", "")
						.replaceAll("[\\s]{2,}", " ");
		System.out.println(this.body);
	}
	
	public String getBody() {
		return body;
	}
	
	public String[] getBodyWords() {
		return body.split(" ");
	}
}
