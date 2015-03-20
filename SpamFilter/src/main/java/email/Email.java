package email;

public class Email {
	//TODO - add url links
	private String sender;
	private String subject;
	private String body;
	
	public Email(String sender, String subject, String body) {
		if(body == null || sender == null || subject == null)
			throw new IllegalArgumentException("Message arguments cannot be null");
		
		this.body = processText(body);
		this.subject = processText(subject);
		this.sender = processText(sender);
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
