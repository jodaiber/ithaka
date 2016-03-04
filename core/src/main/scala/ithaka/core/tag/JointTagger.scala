package ithaka.core.tag

/**
 * Any part-of-speech tagger implementation providing both fine-grained and coarse-grained tags.
 */
trait JointTagger {

  /**
   * Tag the tokens and provide fine and coarse-grained POS tags.
   *
   * @param tokens the input sentence
   * @param original the original input sentence
   * @param dropGoldTags number of tokens dropped from start and end of the sentence
   * @return
   */
  def tag(tokens: Seq[String], original: Option[String] = None, dropGoldTags: (Int, Int) = (0, 0)): (Array[String], Array[String])

  /**
   * Is the current tagger a gold POS tagger?
   *
   * @return true if gold POS tagger, else false.
   */
  def isGoldTagger: Boolean
}