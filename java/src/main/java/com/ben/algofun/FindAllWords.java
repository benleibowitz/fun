package com.ben.algofun;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Find all words possible made from a given alphabet
 * (find permutations of the alphabet)
 * @author ben
 *
 */
public class FindAllWords {
	/**
	 * Takes an alphabet, and returns a Set of all possible words given that alphabet
	 * (all possible permutations)
	 * @param alphabet
	 * @return Set of all possible words (permutations)
	 */
	public static Set<String> findAll(Character[] alphabet) {
		if(alphabet == null) {
			throw new IllegalArgumentException("Alphabet cannot be null");
		}
		StringBuilder sb = new StringBuilder();
		for(int j = 0; j < alphabet.length; j++) {
			sb.append(' ');
		}
		return find(0, sb, Arrays.stream(alphabet).collect(Collectors.toList()), new HashSet<>());
	}
	
	private static Set<String> find(int idx, StringBuilder word, List<Character> alphabet, Set<String> all) {
		if(idx == word.length() || alphabet.size() == 1) {
			word.setCharAt(word.length() - 1, alphabet.get(0));
			all.add(word.toString());
		} else {
			for(int i = 0; i < alphabet.size(); i++) {
				List<Character> alphabetCopy = new ArrayList<>(alphabet);
				word.setCharAt(idx, alphabetCopy.get(i));
				alphabetCopy.remove(i);
				find(idx + 1, word, alphabetCopy, all);
			}
		}
		return all;
	}
}
