package ithaka.core.tag

import ithaka.core.model.{FineTagger, CoarseTagger}
import cmu.arktweetnlp.impl.{ModelSentence, Sentence}
import cmu.arktweetnlp.Tagger
import scala.collection.JavaConversions._

/**
 * A wrapper for the Twitter-specific part-of-speech tagger described in:
 *
 * Gimpel, Kevin, et al. Part-of-speech tagging for twitter: Annotation, features, and experiments.
 * CARNEGIE-MELLON UNIV PITTSBURGH PA SCHOOL OF COMPUTER SCIENCE, 2010.
 *
 * @param model
 */
class ArkTagger(model: String) extends CoarseTagger with BaseTagger {

  val arkToCoarseMapping = tagMapping("en-tweet.map")

  val arkTagger = new Tagger()
  arkTagger.loadModel(model)

  override def tag(tokens: Seq[String]): Array[String] = {

    val sentence = new Sentence()
    sentence.tokens = tokens.toList

    val ms = new ModelSentence(sentence.T())
    arkTagger.featureExtractor.computeFeatures(sentence, ms)
    arkTagger.model.viterbiDecode(ms)

    (0 until sentence.T()).map { t: Int =>
      arkTagger.model.labelVocab.name( ms.labels(t) )
    }.toArray.map(arkToCoarseMapping)

  }

  def tagNBest(tokens: Seq[String]): Array[Array[String]] = tag(tokens).map(t => Array(t))

}
