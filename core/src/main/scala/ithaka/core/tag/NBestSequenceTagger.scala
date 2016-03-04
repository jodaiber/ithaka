package ithaka.core.tag

import ithaka.core.model.FineTagger
import opennlp.tools.postag.{POSTaggerME, POSTagger}
import scala.collection.JavaConversions._

/**
 * A n-best POS tagger using the OpenNLP tagger implementation.
 *
 * @param tagger OpenNLP tagger
 */
class NBestSequenceTagger(tagger: POSTaggerME) extends FineTagger {

  /**
   * Tag a sequence of tokens with first-best part-of-speech tags.
   *
   * @param tokens the input sentence
   * @return
   */
   def tag(tokens: Seq[String]): Array[String] = tagger.tag(tokens.toArray)

  /**
   * Tag a sequence of tokens with n-best part-of-speech tags.
   *
   * @param tokens the input sentence
   * @return
   */
  def tagNBest(tokens: Seq[String]): Array[Array[String]] = tagger.topKSequences(tokens.toArray).map(s => s.getOutcomes.toList.toArray).transpose

}
