package ithaka.core.tag

import opennlp.tools.postag.{POSModel, DefaultPOSContextGenerator, POSTaggerME}
import ithaka.core.model.FineTagger
import eu.danieldk.nlp.jitar.data.TriGram
import breeze.numerics._
import collection.mutable
import java.io.FileInputStream

/**
 * A n-best POS-tagger based on the OpenNLP ME tagger.
 *
 * @param model
 */

class DiscriminativeTagger(model: POSModel) extends ForwardBackwardTagger {

  val posModel = model.getPosModel
  val contextGen = new DefaultPOSContextGenerator(5, model.getNgramDictionary)

  val tags_ = 0 until posModel.getNumOutcomes
  override def tags() = tags_

  val states_ = (for(t1 <- tags(); t2 <- tags()) yield (t1, t2)).zipWithIndex.toMap
  protected def states() = states_

  protected def t(h1: Int, h2: Int, tag: Int): Double = 0.0
  protected def o(tokens: Array[String], i: Int): Map[Int, Double] = Map[Int, Double]()


  protected def calculateScores(tokens: Array[String], i: Int, history: Array[String]): Map[Int, Double] = {
    val priorDecisions = tokens.zipWithIndex.map{
      case (token: String, j: Int) if j == i-1 => history.last
      case (token: String, j: Int) if j == i-2 => history.head
      case _ => ""
    }

    val contexts = contextGen.getContext(i, tokens, priorDecisions, null)

    posModel.eval(contexts).zipWithIndex.map{
      case (score: Double, tag: Int) => (tag, log(score))
    }.toMap
  }


  protected def tagsForToken(token: String): Seq[Int] = tags()
  protected def tagToString(tag: Int): String = posModel.getOutcome(tag)
  protected def startTag(): Int = -1
  protected def endTag(): Int = -2

  override def forward(observations: Array[String]) = {

    val α = IndexedSeq.fill(observations.size)(
      mutable.IndexedSeq.fill(states().size + 1)(LOGZERO)
    )

    for (i <- 0 until observations.size) {

      for (prevState <- states().keys) {
        val scores = calculateScores(observations, i, Array(tagToString(prevState._1), tagToString(prevState._2)))

        for (tag <- tagsForToken(observations(i))) {

          α(i)( states()( (prevState._2, tag) ) ) = if (i == 0)
            scores(tag)
          else
            logSum(
              α(i)( states()( (prevState._2, tag) ) ),
              α(i - 1)( states()( (prevState._1, prevState._2) ) ) + scores(tag)
            )
        }
      }
    }

    α.map(_.toIndexedSeq)
  }

  override def backward(observations: Array[String]) = {

    val β = IndexedSeq.fill(observations.size)(
      mutable.IndexedSeq.fill(states().size + 1)(LOGZERO)
    )

    for (i <- (observations.size-1) to 0 by -1) {

      if (i == observations.size - 1) {
        for (prevState <- states().keys)
          β(i)( states()(prevState) ) = 0.0
      } else {
        val tagsForI = tagsForToken(observations(i+1))
        for (prevState <- states().keys) {

          val scores = calculateScores(observations, i+1, Array(tagToString(prevState._1), tagToString(prevState._2)))

          for (tag <- tagsForI) {
            β(i)( states()(prevState) ) = logSum(
              β(i)( states()(prevState) ),
              β(i + 1)( states()( (prevState._2, tag) ) ) + scores(tag)
            )
          }
        }
      }
    }

    β.map(_.toIndexedSeq)
  }



  override def tag(tokens: Seq[String]): Array[String] = {
    tagNBestWithScore(tokens).map(_.head._1)
  }
}

object DiscriminativeTagger {

  def main(args: Array[String]) {
    val tagger = new DiscriminativeTagger(new POSModel(new FileInputStream("lib/en-pos-maxent.bin")))
    println( tagger.tagNBestWithScore("Peter saw the girl .".split(" ")).map(_.take(3).mkString("|")).mkString(" ") )
  }

}