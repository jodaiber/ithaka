package ithaka.core.normalize

import ithaka.core.model.{Tokens, Token, TokenNormalizer}
import java.io.File
import scala.Array
import ithaka.core.tag.GoldTagger

/**
 * Provides gold normalizations for token normalizers.
 *
 * @param tokensFile file containing the original tokens
 * @param goldFile file containing the normalized tokens and their alignment
 */
class GoldTokenNormalizer(tokensFile: File, goldFile: File) extends TokenNormalizer {

  val goldInstanceForTokens = GoldTagger.getGoldInstances(tokensFile, goldFile)

  /**
   * Get first-best token normalization for the input text.
   *
   * @param text input sentence
   * @return
   */
  def firstBest(text: String): Array[Token] = Tokens.fromSentence( goldInstanceForTokens(text).forms.tail.toArray )


  /**
   * Get first-best token normalization for the tokenized input sentence.
   *
   * @param tokens input sentence in Token form
   * @return
   */
  def firstBest(tokens: Array[Token]): Array[Token] = Tokens.fromSentence( goldInstanceForTokens(tokens.map(_.token).mkString(" ")).forms.tail.toArray )


  /**
   * Get n-best token normalization for the input text.
   *
   * @param text input sentence
   * @return
   */
  def nBest(text: String): Array[Array[Token]] = firstBest(text).map(t => Array(t))

  /**
   * Get first-best token normalization for the tokenized input sentence.
   *
   * @param tokens input sentence in Token form
   * @return
   */
  def nBest(tokens: Array[Token]): Array[Array[Token]] = firstBest(tokens).map(t => Array(t))

}
