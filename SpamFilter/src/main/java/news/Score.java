package news;

/*
 * Score enum represents categories
 * which should not be included in news stories.
 * 
 * ie: a real news article should be close to 0
 * for all the below types/categories
 */
public enum Score {
  CELEBRITY, SARCASM, VITRIOL, SPAM,
  PROMOTION, SENSATIONALIST
}
