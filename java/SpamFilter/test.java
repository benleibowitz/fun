package spam;

import java.util.Properties;
import java.util.Scanner;

import javax.mail.Authenticator;
import javax.mail.Folder;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Store;

import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

public class test {
	public static void main(String[] args) {
		
		ApplicationContext c = new ClassPathXmlApplicationContext("beans.xml");
		ProbabilityCalculator probCalc = (ProbabilityCalculator)c.getBean("probabilitycalculator");
		
		Message m = new Message("Online Discount pharmacy", "Big sale!!", "Bestsellers: pain pills viagra");
		System.out.println(probCalc.isSpam(m));
		//Scanner s = new Scanner(System.in);
		//ProbabilityTrainer t = new ProbabilityTrainer();
		//Message m = new Message("Top-quality  -   woo");
		//System.out.println(m.getBody());
		/*
		while(true) {
			System.out.println("message:");
			String inp = s.nextLine();
			if(inp.equals("@")) break;
			t.train(new Message(inp), true);
		}
		t.commit();
		*/
	 }
}
