package comment;

import java.util.Scanner;

import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

public class test {
	public static void main(String[] args) {
		
		ApplicationContext c = new ClassPathXmlApplicationContext("commentbeans.xml");
		ProbabilityCalculator probCalc = (ProbabilityCalculator)c.getBean("commentprobabilitycalculator");
		
		Comment comm = new Comment("fucking libtards");
		System.out.println(probCalc.isSpam(comm));
		Scanner s = new Scanner(System.in);
		BayesCommentProbabilityTrainer t = (BayesCommentProbabilityTrainer)c.getBean("commentprobabilitytrainer");
		//System.out.println(m.getBody());
		
		while(true) {
			System.out.print("comment or @ to break:");
			String se = s.nextLine();
			if(se.equals("@")) break;
			System.out.println("true/false");
			boolean b = s.nextBoolean();
			t.train(new Comment(se), b);
			s.nextLine();
		}
		t.commit();
		
		
	 }
}
