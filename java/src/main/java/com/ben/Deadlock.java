package com.ben;
/*
 *  Example of 2 threads deadlocking because
 *  they both are waiting on the other to exit
 *  a synchronize block
 */

public class Deadlock {
	private static String i1 = "foo";
	private static String i2 = "bar";
	
	public static void main(String[] args) {
		Thread t1 = new Thread(new Runnable() {
			
			@Override
			public void run() {
				synchronized(i2) {
					System.out.println("T1 in sync block for i2");
					try {
						Thread.sleep(500);
					} catch (InterruptedException e) { e.printStackTrace(); }
					
					System.out.println("T1 waiting for free on i1");
					
					synchronized(i1) {
						System.out.println("T1 in sync block for i1");
					}
				}
				System.out.println("T1, all syncs released");

			}
		});
		
		Thread t2 = new Thread(new Runnable() {
			
			@Override
			public void run() {
				synchronized(i1) {
					System.out.println("T2 in sync block for i1");
					try {
						Thread.sleep(500);
					} catch (InterruptedException e) { e.printStackTrace(); }

					System.out.println("T2 waiting for free on i2");
					
					synchronized(i2) {
						System.out.println("T2 in sync block for i2");
					}
				}
				System.out.println("T2, all syncs released");
			}
		});
		
		t1.start();
		t2.start();
	}
    
}
