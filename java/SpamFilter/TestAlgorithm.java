package spam;
import java.util.Map;

class TestAlgorithm implements SpamAlgorithm {
	@Override
	public boolean isSpam(Message message, ScoringSystem scoringSystem) {
		System.out.println("Test algorithm called on message: " + message.getBody());
		return true;
	}
}
