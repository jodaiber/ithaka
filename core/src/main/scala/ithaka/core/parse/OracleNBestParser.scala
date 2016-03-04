package ithaka.core.parse

import ithaka.core.model._
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import mstparser.{DependencyInstance, DependencyParser}
import ithaka.core.normalize.HanBaldwinTokenNormalizer
import ithaka.core.tag.JointTagger
import scala.Some

/**
 * An oracle dependency parser for evaluating token-based n-best text-normalizers.
 *
 * @param dp the DependencyParser object
 * @param normalizer a text-normalizer
 * @param tagger a part-of-speech tagger combination
 * @param includeOriginalToken should the original (non-normalized) token be included in the n possibilities
 * @param n the number of n-best lexical normalizations considered for each token token
 */
class OracleNBestParser(dp: DependencyParser, normalizer: TokenNormalizer, tagger: JointTagger, val includeOriginalToken: Boolean = false, val n: Int = 10) extends MSTParser(dp, tagger) {

  override def name = "%s %s, %s n=%d +original=%b".format(this.getClass.getSimpleName, normalizer.getClass.getSimpleName, tagger.toString, n, includeOriginalToken)

  /**
   * Generate all possible permutations of the n-best normalized tokens for a sentence.
   *
   * @param sent the sentence to be parsed
   * @return
   */
  def nbestNormalizations(sent: String): Array[Array[Token]] = {
    val tokens = Tokens.fromTokenizedString(sent)
    val nbest: Array[Array[Token]] = normalizer.nBest( tokens ).map(_.take(n))

    if (includeOriginalToken)
      tokens.zipWithIndex.foreach{
        case (t: Token, i: Int) =>
          if (!nbest(i).contains(t))
            nbest(i) :+= t
      }

    nbest
  }

  /**
   * Return the n-best parses for the sentence.
   *
   * @param sent input sentence to be parsed
   * @return
   */
  override def parseNBest(sent: String): Seq[DependencyInstance] = {

    val nbest = nbestNormalizations(sent)

    HanBaldwinTokenNormalizer.iterateAll( nbest ).map {
      tokens: Array[Token] =>
        parse( tokens.map(_.token).mkString(" "), original = Some(sent) )
    }

  }

  /**
   * Return the n-best parses for the sentence (as a dependency instance).
   *
   * @param instance input sentence to be parsed
   * @return
   */
  override def parseNBest(instance: DependencyInstance): Seq[DependencyInstance] = {
    throw new NotImplementedException()
  }


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
    (parseNBest(sent).head, Alignment.oneToOne(sent))
  }
}
