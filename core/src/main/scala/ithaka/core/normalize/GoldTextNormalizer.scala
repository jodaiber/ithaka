package ithaka.core.normalize

import ithaka.core.model.{Alignment, TextNormalizer}
import java.io.File
import io.Source

/**
 * A text-normalizer providing the gold normalizations from the corpus.
 *
 * @param tokensFile file containing the original tokens
 * @param alignmentFile file containing the normalized tokens and their alignment
 */
class GoldTextNormalizer(tokensFile: File, alignmentFile: File) extends TextNormalizer {

  val goldNormalizations = GoldTextNormalizer.getGoldInstancesWithAlignment(tokensFile, alignmentFile)

  def firstBest(tokenizedString: String): (Array[String], Alignment) = {
    goldNormalizations(tokenizedString.trim)
  }

}

object GoldTextNormalizer {


  /**
   * Load the gold instances and alignment from the normalization file.
   *
   * @param tokensFile file containing the original tokens
   * @param alignmentFile file containing the normalized tokens and their alignment
   * @return
   */
  def getGoldInstancesWithAlignment(tokensFile: File, alignmentFile: File): Map[String, (Array[String], Alignment)] = {
    Source.fromFile(tokensFile, "utf-8").getLines().map(l => l.trim).zip(
      Source.fromFile(alignmentFile, "utf-8").getLines().map(Alignment.tokensFromLine(_)).zip(
        Source.fromFile(alignmentFile, "utf-8").getLines().map(Alignment.fromLine(_)))
    ).toMap
  }



}
