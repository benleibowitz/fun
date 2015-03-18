package spam;

class TestAlgorithm implements SpamAlgorithm {
	@Override
	public boolean isSpam(Message message) {
		System.out.println("Test algorithm called on message: " + message.getBody());
		return true;
	}
}
