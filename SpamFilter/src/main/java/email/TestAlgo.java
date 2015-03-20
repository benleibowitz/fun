package email;


public class TestAlgo implements SpamAlgorithm {
	@Override
	public boolean isSpam(Email email) {
		System.out.println("Test algorithm");
		return true;
	}
}
