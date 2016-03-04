package ithaka.core.tag

import ithaka.core.model.FineTagger
import edu.stanford.nlp.tagger.maxent.MaxentTagger

/**
 * Fine POS tagger using the Stanford POS tagger.
 *
 * @param tagger Stanford MaxentTagger
 */
class StanfordFirstBestTagger(tagger: MaxentTagger) extends FineTagger {

  /**
   * Tag a sequence of tokens with first-best part-of-speech tags.
   *
   * @param tokens the input sentence
   * @return
   */
  def tag(tokens: Seq[String]): Array[String] = tagger.tagTokenizedString(tokens.mkString(" ")).split(" ").map(_.split("_")(1))

  /**
   * Tag a sequence of tokens with n-best part-of-speech tags.
   *
   * @param tokens the input sentence
   * @return
   */
  def tagNBest(tokens: Seq[String]): Array[Array[String]] = tag(tokens).map(t => Array(t))


}
