package ithaka.core.tag

import opennlp.tools.postag.{POSModel, DefaultPOSContextGenerator, POSTaggerME}
import ithaka.core.model.FineTagger
import java.io.FileInputStream


/**
 * A unigram n-best tagger based on the OpenNLP ME tagger.
 *
 * @param model
 */

class DiscriminativeUnigramTagger(model: POSModel) extends POSTaggerME(model) with FineTagger {

  def tag(tokens: Seq[String]): Array[String] = tag(tokens.toArray)

  posModel = model.getPosModel
  contextGen = new DefaultPOSContextGenerator(5, model.getNgramDictionary)

  override def tagNBestWithScore(tokens: Seq[String]): Array[Array[(String, Double)]] = {

    tokens.zipWithIndex.map{case (token: String, i: Int) =>
      val contexts = contextGen.getContext(i, tokens.toArray, tokens.toArray.map(_ => ""), null)
      posModel.eval(contexts).zipWithIndex.map{
        case (score: Double, i: Int) =>
          (posModel.getOutcome(i), score)
      }.sortBy(_._2).reverse.take(20)
    }.toArray
  }


  def tagNBest(tokens: Seq[String]): Array[Array[String]] =
    tagNBestWithScore(tokens).map(_.map(_._1))

}

object DiscriminativeUnigramTagger {
  def main(args: Array[String]) {
    val tagger = new DiscriminativeUnigramTagger(new POSModel(new FileInputStream("lib/en-pos-maxent.bin")))
    println( tagger.tagNBestWithScore("Peter saw the girl .".split(" ")).map(_.mkString("|")).mkString(" ") )
  }
}
