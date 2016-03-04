package ithaka.core.normalize

import ithaka.core.model.{Alignment, TextNormalizer}
import java.io.File
import scala.io.Source

/**
 * Normalizer using a normalization dictionary.
 *
 * @param dict internal dictionary representation
 */
class DictionaryNormalizer(dict: Map[String, String]) extends TextNormalizer {


  /**
   * Provide first-best normalization and the alignment for the tokenized, space-delimited
   * input sentence.
   *
   * @param tokenizedString the tokenized input sentence
   * @return
   */
  def firstBest(tokenizedString: String): (Array[String], Alignment) = {
    (tokenizedString.split(" ").map(t => dict.getOrElse(t, t)), Alignment.oneToOne(tokenizedString))
  }

}

object DictionaryNormalizer {

  /**
   * Load the normalization dictionary from a dictionary file in the format
   *
   * lexical variant \t correct term
   *
   * @param file normalization dictionary file
   * @return
   */
  def fromFile(file: File): DictionaryNormalizer = {
    new DictionaryNormalizer(Source.fromFile(file).getLines().map(l => l.split("\t")).map( l => (l(0), l(1)) ).toMap)
  }

}
