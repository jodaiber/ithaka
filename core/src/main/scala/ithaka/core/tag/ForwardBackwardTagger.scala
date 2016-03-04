package ithaka.core.tag

import collection.mutable
import breeze.numerics._
import ithaka.core.model.FineTagger

/**
 * A base class for POS taggers implementing n-best part-of-speech tagging
 * using the forward-backward algorithm.
 */

trait ForwardBackwardTagger extends FineTagger {

  //Number of tags for each token considered in the trellis
  protected def k: Int = 3


  /**
   * Provide the transition probability for tag with history h1 and h2
   *
   * @param h1 tag history
   * @param h2 tag history
   * @param tag tag
   * @return
   */
  protected def t(h1: Int, h2: Int, tag: Int): Double


  /**
   * Provide the emission probability for all tags for a token at position i.
   *
   * @param tokens all tokens in the sentence
   * @param i the index of token of interest
   * @return
   */
  protected def o(tokens: Array[String], i: Int): Map[Int, Double]


  /**
   * Provide the sequence of tag IDs used by the tagger.
   *
   * @return
   */
  protected def tags(): Seq[Int]


  /**
   * Provide the list of possible tags for a token.
   *
   * @param token the token
   * @return
   */
  protected def tagsForToken(token: String): Seq[Int]


  /**
   * Convert a tag ID to the String representation of the tag.
   *
   * @param tag the tag ID
   * @return
   */
  protected def tagToString(tag: Int): String


  /**
   * ID of the specific tag used at the start of the sequence.
   *
   * @return
   */
  protected def startTag(): Int


  /**
   * ID of the specific tag used at the end of the sequence.
   *
   * @return
   */
  protected def endTag(): Int


  /**
   * Map of sates and their IDs.
   *
   * @return
   */
  protected def states(): Map[(Int, Int), Int]


  /**
   * Tag a sequence of tokens with n-best part-of-speech tags.
   *
   * @param tokens the input sentence
   * @return
   */
  def tagNBest(tokens: Seq[String]): Array[Array[String]] =
    tagNBestWithScore(tokens).map(_.map(_._1))


  /**
   * Tag a sequence of tokens with n-best part-of-speech tags and a score for each tag.
   *
   * @param tokens the input sentence
   * @return
   */
  override def tagNBestWithScore(tokens: Seq[String]): Array[Array[(String, Double)]] = {

    val α = forward(tokens.toArray)
    val β = backward(tokens.toArray)

    tokens.zipWithIndex.map{ case(token: String, i: Int) =>
      tags().map({ tag: Int =>
        (tag, logSum( states().toList.map{
          case (t: (Int, Int), s: Int) if(t._2 == tag) => α(i)( s ) + β(i)( s )
          case _ => LOGZERO
         })
        )
      }).filter(!_._2.isNegInfinity).sortBy(_._2).reverse.map{
        case (tag, score) => (tagToString(tag), exp(score))
      }.toArray
    }.toArray
  }

  val LOGZERO = Double.NegativeInfinity

  //Calculate thr forward probability
  protected def forward(observations: Array[String]) = {

    val α = IndexedSeq.fill(observations.size)(
      mutable.IndexedSeq.fill(states().size + 1)(LOGZERO)
    )

    for (i <- 0 until observations.size) {
      val os = o(observations, i)

      for (tag <- tagsForToken(observations(i)).sortBy(t => os(t)).reverse.take(k)) {
        for (prevState <- states().keys) {

          α(i)( states()( (prevState._2, tag) ) ) = if (i == 0)
            t(startTag(), prevState._2, tag) + os(tag)
          else
            logSum(
              α(i)( states()( (prevState._2, tag) ) ),
              α(i - 1)( states()( (prevState._1, prevState._2) ) ) + t(prevState._1, prevState._2, tag) + os(tag)
            )
        }
      }
    }

    α.map(_.toIndexedSeq)
  }

  //Calculate thr backward probability
  protected def backward(observations: Array[String]) = {

    val β = IndexedSeq.fill(observations.size)(
      mutable.IndexedSeq.fill(states().size + 1)(LOGZERO)
    )

    for (i <- (observations.size-1) to 0 by -1) {
      val os = if (i == observations.size - 1) null else o(observations, i+1)

      if (i == observations.size - 1) {
        for (prevState <- states().keys)
          β(i)( states()(prevState) ) = t(prevState._1, prevState._2, endTag())
        } else {
          val tagsForI = tagsForToken(observations(i+1)).sortBy(t => os(t)).reverse.take(k)
          for (prevState <- states().keys) {
            for (tag <- tagsForI) {
              β(i)( states()(prevState) ) = logSum(
                β(i)( states()(prevState) ),
                β(i + 1)( states()( (prevState._2, tag) ) ) + t(prevState._1, prevState._2, tag) + os(tag)
              )
            }
          }
        }
    }

    β.map(_.toIndexedSeq)
  }

}
