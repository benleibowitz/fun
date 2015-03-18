/*
 * ScoringSystem is a marker interface, used to signal
 * that subclasses are scoring systems for their algorithms.
 * For example, you can implement ScoringSystem and
 * define spam / real probabilty mappings for words in an email,
 * or certain ways of scoring words in emails
 */
package spam;

public interface ScoringSystem {
}
