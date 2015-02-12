import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class Java8 {
	public static void main(String[] args) {
		List<String> list = new ArrayList<>();
		list.add("foo");list.add("bar");list.add("penny");list.add("apple");list.add("bar");
		useIterate(list);
		useJava8ForEach(list);
	}
	
	//Older style Java iterator
	public static void useIterate(List<String> list) {
		Iterator<String> iter = list.iterator();
		
		while(iter.hasNext()) {
			String st = (String)iter.next();
			System.out.println(st);
		}
	}
	
	//Java 8 style For Each loop
	public static void useJava8ForEach(List<String> list) {
		Map<String, Integer> occurancesOfItem = new HashMap<>();
		
		list
			.stream()
			.forEach(item -> {
				System.out.println("Item in list: " + item);
				
				if(!occurancesOfItem.containsKey(item))
					occurancesOfItem.put(item, 1);
				else
					occurancesOfItem.put(item, occurancesOfItem.get(item) + 1);
			});
		
		
		//print occurances map
		occurancesOfItem
			.keySet()
			.forEach(key -> System.out.println(key + " occurs: " + occurancesOfItem.get(key) + " times."));
	}
}
