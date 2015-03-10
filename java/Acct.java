/*
 * Abstract class of Acct, which represents a simple bank account object.
 * In implementing this object's subclasses, user must set account currency (either
 * through constructor or setter) before making transactions.
*/

import java.util.Currency;
import java.util.Locale;

public abstract class Acct {
	public class AccountNotInitializedException extends Exception {
		/*
		 * If account is not setup properly (ie, currency not set)
		 */
		AccountNotInitializedException() {
			super();
		}
		AccountNotInitializedException(String message) {
			super(message);
		}
	}
	
	public class IllegalAccountActivityException extends Exception {
		/*
		 * Thrown when attempting to deposit or withdraw a negative number
		 * (withdrawls and deposits must be positive)
		 */
		IllegalAccountActivityException() {
			super();
		}
		IllegalAccountActivityException(String message) {
			super(message);
		}
	}
	
	public enum Curr {
		USD, EUR, GBP
	}
	
	protected int bal;
	protected int monthlyContributionLim;
	protected int monthlyContributionTotal;
	protected Curr curr;
	protected boolean setCurrency;
	protected boolean canUse;
	
	Acct() {
		init();
	}

	public abstract void setMonthlyContributionLim();
	
	protected void init() {
		setCurrency = false;
		canUse = false;
	}
	
	public void setCurrency(Curr curr) throws AccountNotInitializedException {
		if(curr != null) {
			this.curr = curr;
			setCurrency = true;
			canUse = true;
		} else
			throw new AccountNotInitializedException("Cannot set null currency");
	}
	
	//If currency not set properly, throws AccountNotInitializedException
	public String getCurrencySymbol() throws AccountNotInitializedException {
		String symb;
		
		if (setCurrency) {
			Locale locale;
			Currency cur;
			
			switch(curr) {
			case USD:
				locale = Locale.US;
				break;
			case EUR:
				locale = Locale.FRANCE;
				break;
			case GBP:
				locale = Locale.UK;
				break;
			default:
				throw new AccountNotInitializedException("Currency not set");
			}
	
			cur = Currency.getInstance(locale);
			symb = cur.getSymbol(locale);
		} else
			throw new AccountNotInitializedException("Currency not set");
		
		return symb;
	}
	
	//Returns the currency code for the account (ie, EUR, USD).
	//If currency not set properly, throws AccountNotInitializedException
	public String getCurrencyCode() throws AccountNotInitializedException {
		String symb;

		if (setCurrency) {
			Locale locale;
			Currency cur;
			
			switch(curr) {
			case USD:
				locale = Locale.US;
				break;
			case EUR:
				locale = Locale.FRANCE;
				break;
			case GBP:
				locale = Locale.UK;
				break;
			default:
				throw new AccountNotInitializedException("Currency not set");
			}
	
			cur = Currency.getInstance(locale);
			symb = cur.getCurrencyCode();
		} else
			throw new AccountNotInitializedException("Currency not set");
		
		return symb;
	}
	
	public void setBal(int bal) throws AccountNotInitializedException {
		if(canUse)
			this.bal = bal;
		else
			throw new AccountNotInitializedException("Account not setup properly. Try setting currency.");
		
	}
	
	public int getBal() throws AccountNotInitializedException {
		int balance;
		
		if(canUse)
			balance = bal;
		else
			throw new AccountNotInitializedException("Account not setup properly. Try setting currency.");
		return balance;
	}
	
	public boolean deposit(int deposit) throws AccountNotInitializedException, IllegalAccountActivityException {
		boolean depositOK;
		
		if(deposit < 0)
			throw new IllegalAccountActivityException("Deposit cannot be less than 0");
		
		if(canUse) {
			if((monthlyContributionTotal + deposit) <= monthlyContributionLim) {
				depositOK = true;
				bal += deposit;
				monthlyContributionTotal += deposit;
			} else
				depositOK = false;
		} else
			throw new AccountNotInitializedException ("Account not setup properly");
		
		return depositOK;
	}
	
	public boolean withdraw(int withdrawl) throws AccountNotInitializedException, IllegalAccountActivityException {
		boolean withdrawOK;
		
		if(withdrawl < 0)
			throw new IllegalAccountActivityException("Withdrawl must be greater than 0");
		
		if(canUse) {
			if((bal - withdrawl) >= 0) {
				withdrawOK = true;
				bal -= withdrawl;
			} else
				withdrawOK = false;
		} else
			throw new AccountNotInitializedException("Account not setup properly");
		
		return withdrawOK;
	}
	
	public int getMonthlyContributionLim() throws AccountNotInitializedException {
		int monthlyContr;
		if(canUse)
			monthlyContr = monthlyContributionLim;
		else
			throw new AccountNotInitializedException("Account not setup properly");
		
		return monthlyContr;
	}
	
	public void newMonth() {
		monthlyContributionTotal = 0;
	}
}
