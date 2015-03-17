package spam;
import java.util.Map;

class TestAlgorithm implements SpamAlgorithm {
	@Override
	public boolean isSpam(Message message, Map<String, double[]> probabilityMap) {
		System.out.println("Test algorithm called on message: " + message.getBody());
		return true;
	}
}
