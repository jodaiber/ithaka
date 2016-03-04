package ithaka.core.tag

import eu.danieldk.nlp.jitar.tagger.HMMTagger
import eu.danieldk.nlp.jitar.data.{TriGram, Model}
import eu.danieldk.nlp.jitar.wordhandler.{KnownWordHandler, SuffixWordHandler, WordHandler}
import eu.danieldk.nlp.jitar.languagemodel.{LinearInterpolationLM, LanguageModel}
import java.io.{FileReader, BufferedReader, File}
import scala.collection.JavaConversions._
import eu.danieldk.nlp.jitar.tagger

/**
 * A trigram HMM tagger.
 *
 * @param model
 * @param wordHandler
 * @param languageModel
 * @param beamSize
 */
class FineHMMTagger(model: Model, wordHandler: WordHandler, languageModel: LanguageModel, beamSize: Double)
  extends HMMTagger(model, wordHandler, languageModel, beamSize)
  with ForwardBackwardTagger {

  def tag(tokens: Seq[String]): Array[String] = {
    tagger.HMMTagger.highestProbabilitySequence(
      viterbi( ("<START>" +: "<START>" +: tokens :+ "<END>").toList),
      model
    ).sequence().toList.drop(2).dropRight(1).toArray
  }

  val tags_ = model.numberTags().keys.map(_.toInt).toSeq
  override def tags() = tags_

  val states_ = (for(t1 <- tags(); t2 <- tags()) yield (t1, t2)).zipWithIndex.toMap
  protected def states() = states_

  protected def t(h1: Int, h2: Int, tag: Int): Double = languageModel.triGramProb( new TriGram(h1, h2, tag) )
  protected def o(tokens: Array[String], i: Int): Map[Int, Double]  = wordHandler.tagProbs(tokens(i)).map(t => (t._1.toInt, t._2.toDouble)).toMap
  protected def tagsForToken(token: String): Seq[Int] = wordHandler.tagProbs(token).keys.map(_.toInt).toSeq
  protected def tagToString(tag: Int): String = model.numberTags.get(tag)
  protected def startTag(): Int = wordHandler.tagProbs("<START>").maxBy(_._2)._1
  protected def endTag(): Int = wordHandler.tagProbs("<END>").maxBy(_._2)._1

}

object FineHMMTagger {

  /**
   * Load the HMM tagger from the relevant files.
   *
   * @param lexicon
   * @param ngrams
   * @return
   */
  def fromFiles(lexicon: File, ngrams: File): FineHMMTagger = {

    val model: Model =   Model.readModel(new BufferedReader(new FileReader(lexicon)),
      new BufferedReader(new FileReader(ngrams)))

    val swh = new SuffixWordHandler(model.lexicon(), model.uniGrams(), 2, 5, 10, 10, 10)
    val wh = new KnownWordHandler(model.lexicon(), model.uniGrams(), swh)

    // Create an n-gram language model.
    val lm = new LinearInterpolationLM(model.uniGrams(), model.biGrams(), model.triGrams())

    new FineHMMTagger(model, wh, lm, 1000.0)
  }

}