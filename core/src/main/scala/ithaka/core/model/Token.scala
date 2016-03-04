package ithaka.core.model

import scala.collection.JavaConversions._
import cmu.arktweetnlp.Twokenize
import ithaka.core.Utils.splitByPattern

/**
 * A class representing a token in a sentence.
 *
 * @param token the String rep. of the token
 * @param position the position of the token in the sentence
 * @param context the other tokens in the sentence
 */
class Token(val token: String, val position: Int, var context: Array[Token]) {
  var lemma: String = null
  override def toString: String = token
}

object Tokens {

  /**
   * Create Token objects for each token in a sentence.
   *
   * @param sent the tokenized sentence
   * @return
   */
  def fromSentence(sent: Array[String]): Array[Token] = {
    val tokens = sent.zipWithIndex.map{ p => new Token(p._1, p._2, null) }
    tokens.foreach{ t: Token => t.context = tokens }
    tokens
  }


  /**
   * Create Token objects for each token in a sentence.
   *
   * @param sent the tokenized, space-delimited sentence
   * @return
   */
  def fromTokenizedString(sent: String): Array[Token] = {
    val tokens = sent.split(" ").zipWithIndex.map{ p => new Token(p._1, p._2, null) }
    tokens.foreach{ t: Token => t.context = tokens }
    tokens
  }

  /**
   * Create Token objects from the tokens in a sentence (tokenize with a standard tokenizer)
   * @param post the raw sentence
   * @return
   */
  def fromPost(post: String): Array[Token] = {
    val tokens = Twokenize.tokenizeRawTweetText(post).zipWithIndex.map{ p => new Token(p._1, p._2, null) }.toList

    val sents = splitByPattern(tokens)(_.token matches """[.?!]+""")

    sents.foreach {
      sent: List[Token] => {
        val sentA = sent.toArray
        sent.foreach{ t: Token =>
          t.context = sentA
        }
      }
    }

    tokens.toArray
  }


}