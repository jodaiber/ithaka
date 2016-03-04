package ithaka.core.model

/**
 * Detect whether a token is a lexical variant of an IV token.
 *
 * This assumes that the token is not in the vocabulary.
 */

trait IllformedWordDetector {

  /**
   * Decide for a token and a set of IV token suggestions, whether the
   * token is a lexical variant of an IV token.
   *
   * @param token
   * @param candidates
   * @return
   */
  def isIllformed(token: Token, candidates: Array[Confusable]): Boolean

}
