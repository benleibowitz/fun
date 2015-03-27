package email;


public class TestAlgo implements SpamAlgorithm {
	@Override
	public boolean isSpam(Email email) {
		
		//I call this one, AssumptionAlgorithm :)
		System.out.println("Running test algorithm");
		return true;
	}
}
