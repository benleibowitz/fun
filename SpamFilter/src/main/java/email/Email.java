package email;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Email {
	//private String links;
	private String sender;
	private String subject;
	private String body;
	
	public Email(String sender, String subject, String body) {
		if(body == null || sender == null || subject == null)
			throw new IllegalArgumentException("Message arguments cannot be null");

		//links = "";
		this.body = processText(body);
		this.subject = processText(subject);
		this.sender = processText(sender);
		//links.trim();
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
		//processLinks(text);
		
		return s.toLowerCase()
				.replace("-", " ")
				.replaceAll("([.|_|*|^|$|#|@|!|-|+])\\1+", "$1")
				.replaceAll("(?<=[A-Za-z]{2,})[^\\w^\\s]|_(?=[A-Za-z]{1,})", " ")
				.replaceAll("<br>|[^\\w^\\s]|_", "")
				.replaceAll("[\\s]{2,}", " ")
				.replaceAll("(?<=[a-z])(?=[A-Z])", " ")
				//.replaceAll("(?:www|https?)([^\\s])+", "")
				.trim();
	}
	/*
	private void processLinks(String text) {
		Pattern pattern = Pattern.compile("(?:www|https?)([^\\s])+");
		Matcher matcher = pattern.matcher(text);
		
		while(matcher.find()) {
			links += matcher.group();
		}
	}
	*/
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
