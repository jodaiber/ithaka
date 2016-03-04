package ithaka.core.normalize

import ithaka.core.model.{Alignment, TextNormalizer}

/**
 * Text-normalizer combining two separate text-normalizers.
 *
 * @param firstNormalizer first text-normalizer
 * @param secondNormalizer second text-normalizer
 */
class CombinedNormalizer(firstNormalizer: TextNormalizer, secondNormalizer: TextNormalizer) extends TextNormalizer {


  /**
   * Provide first-best normalization and the alignment for the tokenized, space-delimited
   * input sentence.
   *
   * @param tokenizedString the tokenized input sentence
   * @return
   */
  def firstBest(tokenizedString: String): (Array[String], Alignment) = {

    val (firstTokens, firstAlignment) = firstNormalizer.firstBest(tokenizedString)
    val (secondTokens, _) = secondNormalizer.firstBest(firstTokens.mkString(" "))

    (secondTokens, firstAlignment)
  }

}
