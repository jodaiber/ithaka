package ithaka.eval

import java.io.{PrintWriter, FileInputStream, File}
import ithaka.core.parse.{TextNormalizerParser, TwitterParser, MSTParser}
import ithaka.core.tag.{GuessingJointTagger, DiscriminativeMarkovTagger, ArkTagger}
import opennlp.tools.postag.POSModel
import ithaka.core.normalize.moses.MosesNormalizer
import ithaka.core.model.Parser
import EvaluateParsing.evaluateAligned

/**
 * Utility object for performing an error analysis.
 */
object ErrorAnalysis {

  def main(args: Array[String]) {

    //Base Parser
    val parserModel = new File(args(0))
    val dp = MSTParser.fromOptionsFile(parserModel)

    //Coarse taggers:
    val arkTagger = new ArkTagger("lib/model.20120919")

    val maxMarkovTagger = DiscriminativeMarkovTagger.fromFiles(new File("lib/wsj.lexicon"), new File("lib/wsj.ngrams"), new POSModel(new FileInputStream("lib/en-pos-maxent.bin")))

    val tokenFile = new File(args(1))
    val goldFile = new File(args(2))
    val alignmentFile = new File(args(3))

    val bestSetup = new TwitterParser( new TextNormalizerParser(dp, new MosesNormalizer(), new GuessingJointTagger(maxMarkovTagger, arkTagger)) ).asInstanceOf[Parser]

    evaluateAligned(Seq(bestSetup), None, goldFile, tokenFile, alignmentFile, new PrintWriter(System.out, true), new PrintWriter(System.out, true), new File("."), verbose = true)
  }

}
