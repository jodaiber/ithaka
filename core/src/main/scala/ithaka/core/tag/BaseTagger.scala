package ithaka.core.tag

import io.Source

/**
 * Base class for part-of-speech tagger implementations.
 */
trait BaseTagger {

  protected def correctTwitterTags(tokens: Seq[String], tags: Seq[String]): Seq[String] = tokens.zip(tags).map{
    case (token: String, tag: String) if token.startsWith("#") || token.startsWith("@") || token.equals("Urlname") || token.equals("Username") => "NNP"
    case (_, tag: String) => tag
  }

  protected def tagMapping(file: String) = (Source.fromFile(file).getLines().toList.init.map(l => (l.split("\t")(0), l.trim.split("\t")(1))) ++ Array(("<root-CPOS>", "<root-CPOS>"), ("<root-POS>", "<root-POS>"))).toMap.withDefaultValue("?")

}
