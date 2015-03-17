package spam;

import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;


public class test {
	public static void main(String[] args) {
		/*
		Scanner scan = new Scanner(System.in);
		try {
			System.out.print("Enter message:");
			Message m = new Message(scan.nextLine());
			ProbabilityCalculator pc = new ProbabilityCalculator(new NaiveBayesAlgorithm());
			System.out.println("Message is spam: " + pc.isSpam(m));
		} finally {
			scan.close();
		}
		*/
		ApplicationContext c = new ClassPathXmlApplicationContext("beans.xml");
		ProbabilityCalculator probCalc = (ProbabilityCalculator)c.getBean("probabilitycalculator");
		Message m = new Message("click here");
		System.out.println(probCalc.isSpam(m));
	}
}
