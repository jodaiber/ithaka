package ithaka.core.parse

import mstparser._
import java.io.File
import scala.io.Source
import ithaka.core.model.{Alignment, Parser}
import ithaka.core.tag.JointTagger

/**
 * Base class for the MST parser implementation.
 *
 * @param dp the DependencyParser object
 * @param tagger a part-of-speech tagger combination
 */
class MSTParser(val dp: DependencyParser, tagger: JointTagger) extends Parser {

  override def name: String = "%s %s".format(this.getClass.getSimpleName, tagger.toString)

  val decoder = if (dp.options.secondOrder) new DependencyDecoder2O(dp.pipe) else new DependencyDecoder(dp.pipe)

  def parse(sent: String, original: Option[String] = None, dropGoldTags: (Int, Int) = (0, 0)): DependencyInstance = parse(
    instanceFromTokens(sent, original=original, dropGoldTags=dropGoldTags)
  )

  /**
   * Internal parse function.
   *
   * @param instance dummy parse instance
   * @return
   */
  private def parse_(instance: DependencyInstance): Seq[(FeatureVector, (IndexedSeq[Int], IndexedSeq[Int]))] = {
    val (fvs, probs, nt_fvs, nt_probs, fvs_trips, probs_trips, fvs_sibs, probs_sibs) = createArrays(instance.forms.length)
    dp.pipe.fillFeatureVectors(instance, fvs, probs, fvs_trips, probs_trips, fvs_sibs, probs_sibs, nt_fvs, nt_probs, dp.params)

    dp.options.decodeType match {
      case "non-proj" =>
        if (dp.options.secondOrder)
          decoder.asInstanceOf[DependencyDecoder2O].decodeNonProjective(
            instance, fvs, probs,
            fvs_trips, probs_trips,
            fvs_sibs, probs_sibs,
            nt_fvs, nt_probs, dp.options.testK
          )
        else decoder.decodeNonProjective(instance.length, fvs, probs, nt_fvs, nt_probs, dp.options.testK)
      case _ =>
        if (dp.options.secondOrder)
          decoder.asInstanceOf[DependencyDecoder2O].decodeProjective(
            instance.length, fvs, probs,
            fvs_trips, probs_trips,
            fvs_sibs, probs_sibs,
            nt_fvs, nt_probs, dp.options.testK
          )
        else decoder.decodeProjective(instance.length, fvs, probs, nt_fvs, nt_probs, dp.options.testK)
    }
  }

  /**
   * Parse the sentence and return the score and the dependency tree.
   *
   * @param sent the input sentence
   * @param original the original input sentence
   * @return
   */
  def parseWithScore(sent: String, original: Option[String] = None): (Double, DependencyInstance) = parseWithScore(instanceFromTokens(sent, original=original))

  /**
   * Parse the sentence and return the score and the dependency tree.

   * @param instance dummy dependency tree instance
   * @return
   */
  def parseWithScore(instance: DependencyInstance): (Double, DependencyInstance) = {
    val d = parse_(instance)
    val (parse, labels) = d.head._2
    val score = this.dp.params.getScore(d.head._1)
    (score, new DependencyInstance(instance.forms, instance.lemmas, instance.cpostags, instance.postags, null, labels.map(l => if(l < 0) "ROOT" else dp.pipe.typeAlphabet.values(l)), parse, null))
  }

  /**
   * Parse a pre-built dependency instance.
   *
   * @param instance pre-built dependency instance
   * @return instance with unlabeled and labeled dependencies
   */
  def parse(instance: DependencyInstance): DependencyInstance = {
    val (parse, labels) = parse_(instance).head._2
    new DependencyInstance(instance.forms, instance.lemmas, instance.cpostags, instance.postags, null, labels.map(l => if(l < 0) "ROOT" else dp.pipe.typeAlphabet.values(l)), parse, null)
  }

  /**
   * Return the n-best parses for the sentence.
   *
   * @param sent input sentence to be parsed
   * @return
   */
  def parseNBest(sent: String): Seq[DependencyInstance] = parseNBest(instanceFromTokens(sent, original = Some(sent)))

  /**
   * Return the n-best parses for the sentence (as a dependency instance).
   *
   * @param instance input sentence to be parsed
   * @return
   */
  def parseNBest(instance: DependencyInstance): Seq[DependencyInstance] = Seq[DependencyInstance]( parse(instance) )

  //Utility function
  private def createArrays(length: Int) = (
    Array.ofDim[FeatureVector](length, length, 2),
    Array.ofDim[Double](length, length, 2),
    Array.ofDim[FeatureVector](length, dp.pipe.typeAlphabet.size, 2, 2),
    Array.ofDim[Double](length, dp.pipe.typeAlphabet.size, 2, 2),

    Array.ofDim[FeatureVector](length, length, length),
    Array.ofDim[Double](length, length, length),

    Array.ofDim[FeatureVector](length, length, 2),
    Array.ofDim[Double](length, length, 2)
    )

  /**
   * Create an initial dependency instance from a sentence.
   *
   * @param tokenString the sentence
   * @param original original version of the sentence
   * @param dropGoldTags drop gold part-of-speech tags from the sentence in case tokens were removed from the
   *                     start or end of the sentence.
   * @return
   */
  override def instanceFromTokens(tokenString: String, original: Option[String] = None, dropGoldTags: (Int, Int) = (0, 0)): DependencyInstance = {

    val instance = super.instanceFromTokens(tokenString, original)

    val tags = tagger.tag(tokenString.split(" ").toList, original = original, dropGoldTags=dropGoldTags)
    val (posTags, cposTags) = (tags._1.toIndexedSeq, tags._2.toIndexedSeq)

    new DependencyInstance(
      instance.forms,
      instance.lemmas,
      "<root-CPOS>" +: cposTags,
      "<root-POS>" +: posTags,
      instance.feats,
      instance.deprels,
      instance.heads,
      null
    )

  }

  /**
   * Parse the sentence and return a dependency instance that is mappable via the provided
   * alignment to the original sentence.
   *
   * @param sent tokenized, space-delimited sentence
   * @param original original version of the sentence (e.g. if the text was normalized)
   * @param dropGoldTags drop gold part-of-speech tags from the sentence in case tokens were removed from the
   *                     start or end of the sentence.
   * @return
   */
  def parseWithAlignment(sent: String, original: Option[String] = None, dropGoldTags: (Int, Int) = (0, 0)): (DependencyInstance, Alignment) = {
    (parse(sent, original = original, dropGoldTags=dropGoldTags), Alignment.oneToOne(sent))
  }

}


object MSTParser {

  /**
   * Load a trained MST parser model from an options file.
   *
   * @param optsFile the options file
   * @return
   */
  def fromOptionsFile(optsFile: File): DependencyParser = {
    val options = new ParserOptions( Source.fromFile(optsFile).getLines().mkString("").split("(\\]|FLAGS \\[| \\| )").tail.map(_.replace(" ", "")) )

    val pipe =
      if (options.secondOrder) new DependencyPipe2O(options)
      else new DependencyPipe(options)

    val dp = new DependencyParser(pipe, options)
    print("\tLoading model...")
    dp.loadModel(options.modelName)

    dp
  }

}