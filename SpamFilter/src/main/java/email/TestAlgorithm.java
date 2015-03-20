package email;

class TestAlgorithm implements SpamAlgorithm {
	@Override
	public boolean isSpam(Email email) {
		System.out.println("Test algorithm called on message: " + email.getBody());
		return true;
	}
}
