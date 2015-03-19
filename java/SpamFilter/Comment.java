package spam;

public class Comment implements Message {
	private String body;
	
	public Message(String body) {
		if(body == null)
			throw new IllegalArgumentException("Comment body cannot be null");
		
		this.body = processText(body);
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
}
