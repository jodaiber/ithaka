package ithaka.core.model

import mstparser.DependencyInstance

/**
 * Simple base class for a dependency parser.
 */
abstract class Parser {

  /**
   * Each parser has a name.
   * @return
   */
  def name: String = this.getClass.getSimpleName


  /**
   * Parse a tokenized, space-delimited sentence.
   *
   * @param sent tokenized, space-delimited sentence
   * @param original original version of the sentence (e.g. if the text was normalized)
   * @param dropGoldTags drop gold part-of-speech tags from the sentence in case tokens were removed from the
   *                     start or end of the sentence.
   *
   * @return dependency instance for the sentence
   */
  def parse(sent: String, original: Option[String] = None, dropGoldTags: (Int, Int) = (0, 0)): DependencyInstance


  /**
   * Parse a pre-built dependency instance.
   *
   * @param instance pre-built dependency instance
   * @return instance with unlabeled and labeled dependencies
   */
  def parse(instance: DependencyInstance): DependencyInstance

  /**
   * Return the n-best parses for the sentence.
   *
   * @param sent input sentence to be parsed
   * @return
   */
  def parseNBest(sent: String): Seq[DependencyInstance]

  /**
   * Return the n-best parses for the sentence (as a dependency instance).
   *
   * @param instance input sentence to be parsed
   * @return
   */
  def parseNBest(instance: DependencyInstance): Seq[DependencyInstance]


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
  def parseWithAlignment(sent: String, original: Option[String] = None, dropGoldTags: (Int, Int) = (0, 0)): (DependencyInstance, Alignment)

  /**
   * Normalize a token, remove numbers from it.
   *
   * @param s token string
   * @return
   */
  protected def normalize(s: String) =
    if (s.matches("[0-9]+|[0-9]+\\.[0-9]+|[0-9]+[0-9,]+")) "<num>"
    else s

  /**
   * Create an initial dependency instance from a sentence.
   *
   * @param tokenString the sentence
   * @param original original version of the sentence
   * @param dropGoldTags drop gold part-of-speech tags from the sentence in case tokens were removed from the
   *                     start or end of the sentence.
   * @return
   */
  def instanceFromTokens(tokenString: String, original: Option[String] = None, dropGoldTags: (Int, Int) = (0, 0)): DependencyInstance = {

    val tokens = tokenString.split(" ")

    val forms = tokens.map(normalize)
    val pos = tokens.map(_ => "?").toIndexedSeq

    val deprels = tokens.map(_ => "<DUMMY>").toIndexedSeq
    val heads   = tokens.map(_ => -1).toIndexedSeq

    val lemmas = forms.map(form => if (form.length > 5) form.substring(0, 5) else form)
    val cpostags = pos

    val i = new DependencyInstance(
      "<root>" +: forms,
      "<root-LEMMA>" +: lemmas,
      "<root-CPOS>" +: cpostags,
      "<root-POS>" +: pos,
      ("<root>" +: tokens).map(_ => IndexedSeq.empty[String]).toIndexedSeq,
      "<no-type>" +: deprels,
      -1 +: heads,
      null
    )
    i
  }
}
