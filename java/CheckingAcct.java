/*
 * CheckingAcct class, which inherits from abstract class Acct.
 * In implementing this object, user must set account currency (either
 * through constructor or setter) before making transactions.
*/

public class CheckingAcct extends Acct {
	CheckingAcct() {
		initChecking();
		setCurrency = false;
		canUse = false;
	}
	
	CheckingAcct(Curr curr) throws AccountNotInitializedException {
		initChecking();
		
		if(curr != null) {
			this.curr = curr;
			setCurrency = true;
			canUse = true;
		} else
			throw new AccountNotInitializedException("Cannot set null currency");
	}
	
	private void initChecking() {
		setMonthlyContributionLim();
	}
	
	@Override
	public void setMonthlyContributionLim() {
		//for checking accounts, let's set a limit of 20000
		monthlyContributionLim = 20000;
	}
}
