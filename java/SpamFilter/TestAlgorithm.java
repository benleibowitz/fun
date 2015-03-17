package spam;

class TestAlgorithm implements SpamAlgorithm {
	@Override
	public boolean isSpam(Message message, Map<String, double[]> probabilityMap) {
		System.out.println("Running test algorithm on message: " + message.getBody());
		return true;
	}
}
