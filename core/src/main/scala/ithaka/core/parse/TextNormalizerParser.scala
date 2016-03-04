package ithaka.core.parse

import mstparser.{DependencyInstance, DependencyParser}
import ithaka.core.model._
import ithaka.core.normalize.GoldTextNormalizer
import ithaka.core.tag.JointTagger

/**
 * A parser using a text-normalizer to pre-process the input to a dependency parser.
 *
 * @param dp the DependencyParser object
 * @param normalizer the text-normalizer
 * @param tagger a part-of-speech tagger combination
 */
class TextNormalizerParser(dp: DependencyParser, normalizer: TextNormalizer, tagger: JointTagger) extends MSTParser(dp, tagger) {

  override def name = "%s (%s, %s)".format(this.getClass.getSimpleName, normalizer.getClass.getSimpleName, tagger.toString)

  /**
   * Parse the sentence and return a dependency instance that is mappable via the provided
   * alignment to the original sentence.
   *
   * @param sent tokenized, space-delimited sentence
   * @param original original version of the sentence (e.g. if the text was normalized)
   * @param dropGoldTags drop gold part-of-speech tags from the sentence in case tokens were removed from the
   *                     start or end of the sentence.
   * @return
   */
  override def parseWithAlignment(sent: String, original: Option[String] = None, dropGoldTags: (Int, Int) = (0, 0)): (DependencyInstance, Alignment) = {

    val normalization: (Array[String], Alignment) = if (normalizer.isInstanceOf[GoldTextNormalizer]) {
        val n: (Array[String], Alignment) = normalizer.firstBest(original.getOrElse(sent))

        (n._1.drop(dropGoldTags._1).dropRight(dropGoldTags._2), correctAlignment(n._2, dropGoldTags._1, dropGoldTags._2))
      } else {
        normalizer.firstBest(sent)
      }
    (parse(normalization._1.mkString(" "), original=original, dropGoldTags=dropGoldTags), normalization._2)
  }

  /**
   * Correct the alignment provided by the text-normalizer in case domain-specific tokens
   * where removed from the beginning or end of the sentence.
   *
   * @param alignment the text-normalizer alignment
   * @param prefixLength number of tokens removed from the start
   * @param suffixLength number of tokens removed from the end
   * @return
   */
  def correctAlignment(alignment: Alignment, prefixLength: Int, suffixLength: Int): Alignment = {
    val maxS = alignment.alignment.map(_._1).max

    new Alignment(
      alignment.alignment.filter{ case (s: Int, t: Int) =>
        s == 0 || s >  prefixLength && s <= maxS - suffixLength
      }.map{ case (s: Int, t: Int) =>
        (s-prefixLength, t-prefixLength)
      }.toMap
    )
  }

}
