package email;


public class TestAlgo implements SpamAlgorithm {
	@Override
	public boolean isSpam(Email email) {
		
		//I call this one, AssumptionAlgorithm :)
		System.out.println("Running test algorithm on email from sender:" + email.getSender());
		return true;
	}
}
