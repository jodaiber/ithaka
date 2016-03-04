package ithaka.core.tag

import eu.danieldk.nlp.jitar.data.Model
import eu.danieldk.nlp.jitar.wordhandler.{KnownWordHandler, SuffixWordHandler, WordHandler}
import eu.danieldk.nlp.jitar.languagemodel.{LinearInterpolationLM, LanguageModel}
import ithaka.core.model.FineTagger
import scala.collection.JavaConversions._
import java.lang
import breeze.numerics._
import java.io.{FileReader, BufferedReader, File}

/**
 * A unigram n-best tagger based on only the emission probability of an HMM tagger.
 *
 * @param model the HMM tagger model
 * @param wordHandler the HMM word handler
 */

class GenerativeUnigramTagger(model: Model, wordHandler: WordHandler)
  extends FineTagger {

  def tag(tokens: Seq[String]): Array[String] = {
    tagNBest(tokens: Seq[String]).map(_.head)
  }

  def tagNBest(tokens: Seq[String]): Array[Array[String]] =
    tagNBestWithScore(tokens).map(_.map(_._1))

  override def tagNBestWithScore(tokens: Seq[String]): Array[Array[(String, Double)]] = {
    tokens.map{ token: String =>
      wordHandler.tagProbs(token).toList.map{
        case (tag: Integer, score: lang.Double) =>
          (model.numberTags.get(tag), exp(score.toDouble))
      }.sortBy(_._2).reverse.toArray
    }.toArray
  }

}

object GenerativeUnigramTagger {

  /**
   * Load the tagger from the relevant files.
   *
   * @param lexicon the HMM lexicon file
   * @param ngrams the HMM ngrams file
   * @return
   */
  def fromFiles(lexicon: File, ngrams: File): GenerativeUnigramTagger = {

    val model: Model =   Model.readModel(new BufferedReader(new FileReader(lexicon)),
      new BufferedReader(new FileReader(ngrams)))

    val swh = new SuffixWordHandler(model.lexicon(), model.uniGrams(), 2, 5, 10, 10, 10)
    val wh = new KnownWordHandler(model.lexicon(), model.uniGrams(), swh)

    new GenerativeUnigramTagger(model, wh)
  }

}