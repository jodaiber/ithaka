package ithaka.core.model


/**
 * A text-normalizer only performing 1-to-1 normalization.
 */
trait TokenNormalizer {


  /**
   * Get first-best token normalization for the input text.
   *
   * @param text input sentence
   * @return
   */
  def firstBest(text: String): Array[Token]


  /**
   * Get first-best token normalization for the tokenized input sentence.
   *
   * @param tokens input sentence in Token form
   * @return
   */
  def firstBest(tokens: Array[Token]): Array[Token]

  /**
   * Get n-best token normalization for the input text.
   *
   * @param text input sentence
   * @return
   */
  def nBest(text: String): Array[Array[Token]]

  /**
   * Get first-best token normalization for the tokenized input sentence.
   *
   * @param tokens input sentence in Token form
   * @return
   */
  def nBest(tokens: Array[Token]): Array[Array[Token]]

}
