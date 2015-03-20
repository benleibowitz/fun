package comment;

public interface ProbabilityTrainer {
	//Commit / write changes in mapping
	public void commit();
	
	public void train(Comment comment, boolean spam);
}
