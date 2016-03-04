package ithaka.core.tag

import opennlp.tools.postag.{DefaultPOSContextGenerator, POSModel}
import eu.danieldk.nlp.jitar.languagemodel.{LinearInterpolationLM, LanguageModel}
import eu.danieldk.nlp.jitar.data.{Model, TriGram}
import java.io.{FileInputStream, FileReader, BufferedReader, File}
import eu.danieldk.nlp.jitar.wordhandler.{WordHandler, KnownWordHandler, SuffixWordHandler}
import scala.collection.JavaConversions._
import breeze.numerics._


/**
 * A maximum entropy Markov model tagger.
 *
 * @param model the POS model of an OpenNLP ME tagger
 * @param hmmModel the model of an HMM tagger
 * @param wordHandler the word handler of an HMM tagger
 * @param languageModel the language model of an HMM tagger
 */
class DiscriminativeMarkovTagger(model: POSModel, hmmModel: Model, wordHandler: WordHandler, languageModel: LanguageModel) extends ForwardBackwardTagger {

  val posModel = model.getPosModel
  val contextGen = new DefaultPOSContextGenerator(5, model.getNgramDictionary)

  val tags_ = 0 until posModel.getNumOutcomes
  override def tags() = tags_

  val states_ = (for(t1 <- tags(); t2 <- tags()) yield (t1, t2)).zipWithIndex.toMap
  protected def states() = states_

  val hmmTags = hmmModel.tagNumbers()
  val startTag_ = wordHandler.tagProbs("<START>").maxBy(_._2)._1
  val endTag_ = wordHandler.tagProbs("<END>").maxBy(_._2)._1

  private def maxEntTagToHMMTag(t: Int): Int = {
    if(t == startTag())
      startTag_
    else if(t == endTag())
      endTag_
    else
      hmmTags.get(tagToString(t).replace("-LRB-", "(").replace("-RRB-", ")"))
  }

  protected def t(h1: Int, h2: Int, tag: Int): Double = {
    languageModel.triGramProb( new TriGram(maxEntTagToHMMTag(h1), maxEntTagToHMMTag(h2), maxEntTagToHMMTag(tag)) )
  }

  protected def o(tokens: Array[String], i: Int): Map[Int, Double] = {
    val contexts = contextGen.getContext(i, tokens, tokens.map(_ => ""), null)

    posModel.eval(contexts).zipWithIndex.map{
      case (score: Double, tag: Int) => (tag, log(score))
    }.toMap
  }

  protected def tagsForToken(token: String): Seq[Int] = tags()
  protected def tagToString(tag: Int): String = posModel.getOutcome(tag)
  protected def startTag(): Int = -1
  protected def endTag(): Int = -2


  override def tag(tokens: Seq[String]): Array[String] = {
    tagNBestWithScore(tokens).map(_.head._1)
  }
}

object DiscriminativeMarkovTagger {

  /**
   * Load the tagger from the relevant files.
   *
   * @param lexicon the tagger lexicon
   * @param ngrams the tagger ngram model
   * @param posModel the OpenNLP ME model
   * @return
   */
  def fromFiles(lexicon: File, ngrams: File, posModel: POSModel): DiscriminativeMarkovTagger = {

    val model: Model =   Model.readModel(new BufferedReader(new FileReader(lexicon)),
      new BufferedReader(new FileReader(ngrams)))


    val swh = new SuffixWordHandler(model.lexicon(), model.uniGrams(), 2, 5, 10, 10, 10)
    val wh = new KnownWordHandler(model.lexicon(), model.uniGrams(), swh)

    val lm = new LinearInterpolationLM(model.uniGrams(), model.biGrams(), model.triGrams())

    new DiscriminativeMarkovTagger(posModel, model, wh, lm)
  }

}
