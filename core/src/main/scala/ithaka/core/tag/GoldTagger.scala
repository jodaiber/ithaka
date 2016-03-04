package ithaka.core.tag

import ithaka.core.model.FineTagger
import mstparser.io.DependencyReader
import java.io.File
import io.Source
import mstparser.DependencyInstance

/**
 * A gold part-of-speech tagger providing only the gold tag for each token in the relevant sentence.
 *
 * @param tokensFile
 * @param goldFile
 */
class GoldTagger(tokensFile: File, goldFile: File) extends FineTagger {

  val goldInstanceForTokens = GoldTagger.getGoldInstances(tokensFile, goldFile)

  def tag(tokens: Seq[String]): Array[String] = {
    goldInstanceForTokens(tokens.mkString(" ")).postags.tail.toArray
  }

  def tagNBest(tokens: Seq[String]): Array[Array[String]] = tag(tokens).map(t => Array(t))

}

object GoldTagger {

  /**
   * Load the POS tags for each sentence in the test data.
   *
   * @param tokensFile file containing the original tokens
   * @param goldFile file containing the gold annotations in CoNLL format
   * @return
   */
  def getGoldInstances(tokensFile: File, goldFile: File): Map[String, DependencyInstance] = {
    val goldReader = DependencyReader.createDependencyReader("CONLL")
    goldReader.startReading( goldFile.getAbsolutePath )
    Source.fromFile(tokensFile, "utf-8").getLines().map(l => l.trim).zip(goldReader).toMap
  }


}