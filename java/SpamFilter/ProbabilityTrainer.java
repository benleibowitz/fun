package spam;

public interface ProbabilityTrainer {
	//Commit / write changes in mapping
	public void commit();
	
	public void train(Message message, boolean spam);
}
