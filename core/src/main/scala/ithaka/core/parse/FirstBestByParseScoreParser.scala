package ithaka.core.parse

import ithaka.core.model.{FineTagger, Token, TokenNormalizer}
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import edu.stanford.nlp.tagger.maxent.MaxentTagger
import mstparser.{DependencyInstance, DependencyParser}
import ithaka.core.normalize.HanBaldwinTokenNormalizer
import ithaka.core.tag.JointTagger


/**
 * Simple parser ranking n-best normalization by their parsing score.
 *
 **/
class FirstBestByParseScoreParser(dp: DependencyParser, normalizer: TokenNormalizer, tagger: JointTagger, includeOriginalToken: Boolean = false) extends OracleNBestParser(dp, normalizer, tagger, includeOriginalToken=includeOriginalToken, n=1) {

  override def parseNBest(sent: String): Seq[DependencyInstance] = {

    val nbest = nbestNormalizations(sent)

    val parses = HanBaldwinTokenNormalizer.iterateAll( nbest ).map {
      tokens: Array[Token] =>
        parseWithScore( tokens.map(_.token).mkString(" "), original = Some(sent) )
    }

    parses.sortBy(_._1).takeRight(1).map(_._2)
  }

  override def parseNBest(instance: DependencyInstance): Seq[DependencyInstance] = {
    throw new NotImplementedException()
  }

}
