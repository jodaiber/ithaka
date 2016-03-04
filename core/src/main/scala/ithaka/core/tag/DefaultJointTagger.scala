package ithaka.core.tag

import ithaka.core.model.{FineTagger, CoarseTagger}

/**
 * Part-of-speech tagger providing coarse-grained and fine-grained POS tags.
 *
 * If fineTagger and coarseTagger are provided, each is individually used to tag.
 * If only fineTagger is provided, the coarse tags are automatically created from the fine-grained tags.
 *
 * @param fineTagger a fine-grained part-of-speech tagger
 * @param coarseTagger a coarse-grained part-of-speech tagger (optional)
 * @param coarseTagRawText should the raw, non-normalized version of the input be tagged by the coarse-grained tagger?
 */
class DefaultJointTagger(fineTagger: FineTagger, coarseTagger: CoarseTagger, coarseTagRawText: Boolean = true) extends JointTagger with BaseTagger {

  val tagToCoarseMapping = tagMapping("en-ptb.map")

  def this(fineTagger: FineTagger) {
    this(fineTagger, null)
  }

  def tag(tokens: Seq[String], original: Option[String] = None, dropGoldTags: (Int, Int) = (0, 0)): (Array[String], Array[String]) = {

    val (fineTags, coarseTags) = if(this.isGoldTagger) {

      val fineTags = fineTagger.tag(original.get.split(" ")).drop(dropGoldTags._1).dropRight(dropGoldTags._2)
      (fineTags, fineTags.map(tagToCoarseMapping))

    } else {
      val fineTags = fineTagger.tag(tokens)

      val coarseTags = if (coarseTagger == null)
        fineTags.map(tagToCoarseMapping)
      else
        coarseTagger.tag(tokens)

      (fineTags, coarseTags)
    }

    val u = fineTags.zip(coarseTags).map {
      case (fineTag: String, coarseTag: String) =>
        if (coarseTag.equals("?"))
          (fineTag, tagToCoarseMapping(fineTag))
        else
          (fineTag, coarseTag)
    }.unzip

    (u._1.toArray, u._2.toArray)
  }

  override def toString = "JointTagger[%s, %s] (%s)".format(fineTagger.toString, if (coarseTagger==null) "_" else coarseTagger.toString, if (coarseTagRawText) "coarse-tagged raw text" else "coarse-tagged normalized text")

  override def isGoldTagger: Boolean = fineTagger.isInstanceOf[GoldTagger]
}
