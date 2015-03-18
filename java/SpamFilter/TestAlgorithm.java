package spam;
import java.util.Map;

class TestAlgorithm implements SpamAlgorithm {
	@Override
	public boolean isSpam(Message message, Map<String, double[]> bodyProbabilityMap,
			Map<String, double[]> senderProbabilityMap,
			Map<String, double[]> subjectProbabilityMap) {
		System.out.println("Test algorithm called on message: " + message.getBody());
		return true;
	}
}
