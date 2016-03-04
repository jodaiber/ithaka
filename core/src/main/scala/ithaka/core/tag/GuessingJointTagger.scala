package ithaka.core.tag

import ithaka.core.model.{CoarseTagger, FineTagger}

/**
 * A POS tagger implementation combining a domain-specific POS tagger with a
 * general n-best POS tagger.
 *
 * Coarse-grained POS tags are, by default, provided in the universal POS tagset.
 *
 * @param fineTagger the fine-grained, general, n-best POS tagger
 * @param coarseTagger the coarse-grained POS tagger
 * @param coarseTagRawText should the raw, non-normalized version of the input be tagged by the coarse-grained tagger?
 */
class GuessingJointTagger(fineTagger: FineTagger, coarseTagger: CoarseTagger, coarseTagRawText: Boolean = true) extends JointTagger with BaseTagger {

  val tagToCoarseMapping = tagMapping("en-ptb.map")

  val coarseToFineMapping = tagToCoarseMapping.groupBy(_._2).map(p => (p._1, p._2.map(_._1).toSeq)).toMap.withDefaultValue(Seq("?"))

  override def tag(tokens: Seq[String], original: Option[String] = None, dropGoldTags: (Int, Int) = (0, 0)): (Array[String], Array[String]) = {

    val tokensToTag = if (fineTagger.isInstanceOf[GoldTagger])
      original.get.split(" ")
    else
      tokens.toArray

    val coarseTags = coarseTagger.tag(tokensToTag)
    val topK = fineTagger.tagNBest(tokensToTag)

    val fineTags = correctTwitterTags(tokensToTag, coarseTags.map(coarseToFineMapping).zipWithIndex.map{
      p: Pair[Seq[String], Int] =>
        val mappedTwitterTags: Seq[String]   = p._1
        val topKTags: Array[java.lang.String] = topK(p._2)
        val topSingleTag = topK(p._2).head

        if(mappedTwitterTags.head.equals("?"))
          topKTags.head
        else
          topKTags.collectFirst {
            case topKTag: String if mappedTwitterTags.exists( topKTag.equals(_) ) => topKTag
          } match {
            case Some(t) => t
            case None => topSingleTag
          }
    }).toArray

    (fineTags, (coarseTags.zip(fineTags).map{
      case (ark: String, fineTag: String) if ark.equals("?") => tagToCoarseMapping(fineTag)
      case (ark: String, _) => ark
    }))
  }

  override def isGoldTagger: Boolean = false

  override def toString = "CoarseGuessingTagger[%s, %s] (%s)".format(fineTagger.toString, coarseTagger.toString, if (coarseTagRawText) "coarse-tagged raw text" else "coarse-tagged normalized text")

}
