package ithaka.core.model

/**
 * Generic interface for text normalizers.
 */
trait TextNormalizer {

  /**
   * Provide first-best normalization and the alignment for the tokenized, space-delimited
   * input sentence.
   *
   * @param tokenizedString the tokenized input sentence
   * @return
   */
  def firstBest(tokenizedString: String): (Array[String], Alignment)

}
