package ithaka.core.parse

import ithaka.core.model.{FineTagger, Confusable, Token, TokenNormalizer}
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import edu.stanford.nlp.tagger.maxent.MaxentTagger
import ithaka.core.normalize.HanBaldwinTokenNormalizer
import mstparser.{DependencyParser, DependencyInstance}
import ithaka.core.tag.JointTagger

/**
 * Simple parser ranking n-best normalization by both their parsing score and their normalization score.
 *
 **/
class FirstBestByBothScoresParser(dp: DependencyParser, normalizer: TokenNormalizer, tagger: JointTagger, includeOriginalToken: Boolean = false) extends OracleNBestParser(dp, normalizer, tagger, includeOriginalToken=includeOriginalToken, n=1) {

  override def parseNBest(sent: String): Seq[DependencyInstance] = {

    val nbest = nbestNormalizations(sent)

    val parses = HanBaldwinTokenNormalizer.iterateAll( nbest ).map {
      tokens: Array[Token] =>
        val confs = tokens.filter(_.isInstanceOf[Confusable]).map(_.asInstanceOf[Confusable])
        val normalizationScore = confs.map(_.score).sum / confs.size.toDouble
        (normalizationScore, parseWithScore( tokens.map(_.token).mkString(" "), original = Some(sent) ))
    }

    parses.sortBy{
      case (normalizationScore, parse) => normalizationScore * parse._1
    }.takeRight(1).map(_._2._2)
  }

  override def parseNBest(instance: DependencyInstance): Seq[DependencyInstance] = {
    throw new NotImplementedException()
  }

}
