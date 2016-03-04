package ithaka.core.model

import scala.Predef._


/**
 * Rank a set of confusables for an OOV token in a sentence.
 */
trait ConfusionSetRanker {

  /**
   * Rank the set of confusables by their suitability as an IV replacement of the OOV token.
   * @param confusables set of confusable candidates for an OOV token
   * @return
   */
  def rankConfusables(confusables: Set[Confusable]): List[Confusable]

}
