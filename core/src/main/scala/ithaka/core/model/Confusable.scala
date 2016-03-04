package ithaka.core.model

/**
 * A confusable is a candidate normalized IV token for a OOV token in a sentence.
 *
 * @param confusable IV token candidate for the OOV token
 * @param score score for the candidate
 * @param originalToken
 */
class Confusable(confusable: String, var score: Double, originalToken: Token) extends Token(confusable, originalToken.position, originalToken.context) with Ordered[Confusable] {


  /**
   * Define an order on confusables by their score.
   *
   * @param that
   * @return
   */
  def compare(that: Confusable) = this.score.compare(that.score) * -1

  override def toString: String = "Confusable[%s (%.2f) for %s]".format(token, score, originalToken)
}
