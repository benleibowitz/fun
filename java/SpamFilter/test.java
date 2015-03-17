package spam;

import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;


public class test {
	public static void main(String[] args) {
		
		ApplicationContext c = new ClassPathXmlApplicationContext("beans.xml");
		ProbabilityCalculator probCalc = (ProbabilityCalculator)c.getBean("probabilitycalculator");
		
		Message m = new Message("hi you");
		System.out.println(probCalc.isSpam(m));
		
		
	}
}
