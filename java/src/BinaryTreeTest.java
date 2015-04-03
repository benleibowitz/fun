package test;

import java.math.BigDecimal;
import java.util.Random;

import org.junit.Test;
import org.junit.Assert;

import com.BinaryTree;

public class BinaryTreeTest {

	@Test
	public void testAdd() {
		BinaryTree bt = new BinaryTree();
		Random r = new Random();
		for(int i = 0; i < 50; i++) {
			bt.add(new BigDecimal(r.nextInt(40)));
		}
		
	}
	
	@Test
	public void testDepth() {
		BinaryTree bt = new BinaryTree();
		Assert.assertEquals(0, bt.getDepth());
		
		bt.add(new BigDecimal(35));
		Assert.assertEquals(1, bt.getDepth());
		
		bt.add(new BigDecimal(30));
		Assert.assertEquals(2, bt.getDepth());
		
		bt.add(new BigDecimal(49));
		Assert.assertEquals(2, bt.getDepth());
		
		bt.add(new BigDecimal(32));
		Assert.assertEquals(3, bt.getDepth());
		
		bt.add(new BigDecimal(15));
		Assert.assertEquals(3, bt.getDepth());
		
		bt.add(new BigDecimal(63));
		Assert.assertEquals(3, bt.getDepth());
		
		bt.add(new BigDecimal(78));
		Assert.assertEquals(4, bt.getDepth());
		
		bt.add(new BigDecimal(150));
		Assert.assertEquals(5, bt.getDepth());
		
		bt.add(new BigDecimal(70));
		Assert.assertEquals(5, bt.getDepth());
	}

}
