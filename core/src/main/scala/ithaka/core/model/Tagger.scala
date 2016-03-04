package ithaka.core.model

/**
 * Generic interface for part-of-speech taggers.
 */
trait Tagger {

  /**
   * Tag a sequence of tokens with first-best part-of-speech tags.
   *
   * @param tokens the input sentence
   * @return
   */
  def tag(tokens: Seq[String]): Array[String]


  /**
   * Tag a sequence of tokens with n-best part-of-speech tags.
   *
   * @param tokens the input sentence
   * @return
   */
  def tagNBest(tokens: Seq[String]): Array[Array[String]]

  /**
   * Tag a sequence of tokens with n-best part-of-speech tags and a score for each tag.
   *
   * @param tokens the input sentence
   * @return
   */
  def tagNBestWithScore(tokens: Seq[String]): Array[Array[(String, Double)]] = {
    tagNBest(tokens).map(_.map(t => (t, 0.0)))
  }

  override def toString = this.getClass.getSimpleName
}


/**
 * Simple trait for fine-grained POS taggers
 */
trait FineTagger extends Tagger

/**
 * Simple trait for coarse-grained POS taggers
 */
trait CoarseTagger extends Tagger
