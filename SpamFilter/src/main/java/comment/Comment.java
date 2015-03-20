package comment;

public class Comment {
	private String body;
	
	public Comment(String body) {
		if(body == null)
			throw new IllegalArgumentException("Comment body cannot be null");
		
		this.body = processText(body);
	}
	
	private String processText(String text) {
		return text.toLowerCase()
				.replace("-", " ")
				.replaceAll("[.]{1,}", " ")
				.replaceAll("<br>|[^\\w^\\s]|_", "")
				.replaceAll("[\\s]{2,}", " ")
				.trim();
	}
	
	public String getBody() {
		return body;
	}
}
