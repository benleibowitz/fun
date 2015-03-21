package comment;

public class Comment {
	private String body;
	
	public Comment(String body) {
		if(body == null)
			throw new IllegalArgumentException("Comment body cannot be null");
		
		this.body = processText(body);
	}
	
	/*
	* processText(String) takes in a text body and cleans
	* it per regexes below. Punctuation/special chars should be removed,
	* all letters lowercase, periods replcaed with space,
	* and all spaces to one space. example:
	* INPUT:[   foo-bar!..?..$do   #run @run..do run_, run? C_L_I_c_K h_E-Re]
	* OUTPUT:[foo bar do run run do run run click here]
	*/
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
