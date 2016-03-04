package ithaka.core.tag

import ithaka.core.model.FineTagger
import cmu.arktweetnlp.Tagger
import cmu.arktweetnlp.impl.{ModelSentence, Sentence}
import scala.collection.JavaConversions._

class ArkFineTagger(model: String) extends FineTagger with BaseTagger  {

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
    }.toArray

  }

  def tagNBest(tokens: Seq[String]): Array[Array[String]] = tag(tokens).map(t => Array(t))

}
