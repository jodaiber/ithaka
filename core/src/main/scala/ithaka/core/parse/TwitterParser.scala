package ithaka.core.parse

import ithaka.core.model.{Alignment, Parser}
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import mstparser.{RelationalFeature, DependencyInstance}

/**
 * A dependency parser for Twitter. Twitter-specific tokens are
 * treated according to the annotation guidelines.
 *
 * @param parser the underlying parser object
 */
class TwitterParser(parser: Parser) extends Parser {

  val tSpecifics = Set( "RT", "MT", "Username", "Urlname" )

  /**
   * Parse a pre-built dependency instance.
   *
   * @param instance pre-built dependency instance
   * @return instance with unlabeled and labeled dependencies
   */
  def parse(instance: DependencyInstance): DependencyInstance = {
    throw new NotImplementedException()
  }

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
  def parse(sent: String, original: Option[String] = None, dropGoldTags: (Int, Int) = (0, 0)): DependencyInstance = {
    parseWithAlignment(sent, original=original, dropGoldTags)._1
  }

  /**
   * Return the n-best parses for the sentence.
   *
   * @param sent input sentence to be parsed
   * @return
   */
  def parseNBest(sent: String): Seq[DependencyInstance] = Seq( parse(sent) )

  /**
   * Add Twitter-specific tokens to the parse tree provided by the underlying dependency parser.
   *
   * @param tokens the input sentence
   * @param parse the dependency tree for the sentence
   * @param tPrefix tokens removed from the start of the sentence
   * @param tSuffix tokens removed from the end of the sentence
   * @return
   */
  def correctParse(tokens: String, parse: DependencyInstance, tPrefix: Seq[String], tSuffix: Seq[String]): DependencyInstance = {

    val rawInstance = instanceFromTokens(tokens)

    val root = tPrefix.size + parse.deprels.tail.indexOf("ROOT") + 1

    val p = 1 until tPrefix.size+1
    val s = tSuffix.size

    val forms = parse.forms.take(1) ++ p.map(rawInstance.forms(_)) ++ parse.forms.tail ++ rawInstance.forms.takeRight(s)
    new DependencyInstance(
      forms,
      parse.lemmas.take(1) ++  p.map(rawInstance.lemmas(_)) ++ parse.lemmas.tail ++ rawInstance.lemmas.takeRight(s),
      parse.cpostags.take(1) ++ p.map(rawInstance.cpostags(_)) ++ parse.cpostags.tail ++ rawInstance.cpostags.takeRight(s),
      parse.postags.take(1) ++ p.map(rawInstance.postags(_)) ++ parse.postags.tail ++ rawInstance.postags.takeRight(s),
      forms.map(_ => IndexedSeq.empty[String]).toIndexedSeq,
      parse.deprels.take(1) ++ p.map(_ => "DEP") ++ parse.deprels.tail ++ (1 to s).map(_ => "DEP"),
      parse.heads.take(1) ++ p.map(_ => root) ++ parse.heads.tail.map(h => if(h == 0) 0 else tPrefix.size + h ) ++ (1 to s).map(_ => root),
      null
    )
  }


  /**
   * Return the n-best parses for the sentence (as a dependency instance).
   *
   * @param instance input sentence to be parsed
   * @return
   */
  override def parseNBest(instance: DependencyInstance): Seq[DependencyInstance] = {
    throw new NotImplementedException()
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

    val tokens = sent.split(" ")    //Remove Twitter-specific tokens:
    val tPrefix = tokens.takeWhile{t: String => tSpecifics.contains(t) || t.startsWith("@") || t.startsWith("http")}
    val tSuffix = tokens.reverse.takeWhile{t: String => tSpecifics.contains(t) || t.startsWith("#") || t.startsWith("http")}.reverse

    //Parse:
    val cleanedParse = parser.parseWithAlignment( tokens.drop(tPrefix.size).dropRight(tSuffix.length).mkString(" "), original = Some(sent), dropGoldTags = (tPrefix.size, tSuffix.size) )

    //Re-attach Twitter-specific tokens and adjust the alignment:
    (correctParse(sent, cleanedParse._1, tPrefix, tSuffix), correctAlignment(cleanedParse._2, tPrefix, tSuffix))
  }

  /**
   * Correct the alignment of the underlying dependency parser.
   *
   * @param partialAlignment alignment from the dependency parser
   * @param tPrefix tokens removed from the start of the sentence
   * @param tSuffix tokens removed from the end of the sentence
   * @return
   */
  def correctAlignment(partialAlignment: Alignment, tPrefix: Seq[String], tSuffix: Seq[String]): Alignment = {
    val correctedAlignment = partialAlignment.alignment.map{ case (source: Int, target: Int) =>
      (source+tPrefix.size, target+tPrefix.size)
    }.toBuffer

    (0 until tPrefix.size).foreach( i =>
      correctedAlignment += ((i, i))
    )

    val lastSourceToken = correctedAlignment.map(_._1).max
    val lastTargetToken = correctedAlignment.map(_._2).max

    (1 to tSuffix.size).foreach( i =>
      correctedAlignment += ((lastSourceToken+i, lastTargetToken+i))
    )

    new Alignment(correctedAlignment.toMap)
  }

  override def name: String = "TwitterParser[%s]".format( parser.name )

}
