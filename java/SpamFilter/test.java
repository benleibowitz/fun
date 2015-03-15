package spam;

import java.util.Scanner;

public class test {
	public static void main(String[] args) {
		Scanner scan = new Scanner(System.in);
		try {
			System.out.print("Enter message:");
			Message m = new Message(scan.nextLine());
			MessageReader mr = new MessageReader();
			mr.setMessage(m);
			System.out.println("Message is spam: " + mr.isSpam());
		} finally {
			scan.close();
		}
	}
}
