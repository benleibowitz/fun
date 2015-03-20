package email;

public interface ProbabilityTrainer {
	//Commit / write changes in mapping
	public void commit();
	
	public void train(Email email, boolean spam);
}
