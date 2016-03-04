package ithaka.core.model

/**
 * Class representing an alignment between the original tokens and the tokens
 * in a dependency tree.
 *
 * @param alignment internal storage of the alignment in an int-int hashmap
 */
class Alignment(val alignment: Map[Int, Int]) {

  override def toString = alignment.toString()

  /**
   * Retrieve the original token id for the token in the dependency tree.
   *
   * @param i the id of the token in the dependency tree
   * @return original token id
   */
  def sourceNode(i: Int): Int = alignment.getOrElse(i, Alignment.INSERTION)


  /**
   * Retrieve the sequence of tokens mapping to a specific original token.
   *
   * @param i the id of the original token
   * @return sequence of aligned dependency tree tokens
   */
  def targetNodes(i: Int): Seq[Int] = alignment.filter(_._2 == i).map(_._1).toSeq match {
    case s: Seq[Int] if s.isEmpty => Seq(Alignment.DELETION)
    case s: Seq[Int] => s
  }

}

object Alignment {

  //Insertions of tokens, i.e. new tokens without a corresponding original token, use this constant
  val INSERTION = -1

  //Deletions of tokens, i.e. tokens that are deleted from the dependency tree but existent in the
  // original token, use this constant
  val DELETION  = -2

  /**
   * Load a single alignment from the line of a text file. The format is the following:
   *
   * > Why {0} am {1} I {2} up {3} ? {4}
   *
   * @param line line from normalized token file
   * @return
   */
  def fromLine(line: String): Alignment = {
    new Alignment(
      (line.trim.split(" ").grouped(2).zipWithIndex.map { case (tokens, i) =>
        val s = tokens.last.tail.init.toInt
        (i+1, if (s >= 0) s+1 else s)
      }.toSeq :+ (0, 0)).toMap
    )
  }

  /**
   * Provide only the normalized tokens from the line of a normalization file.
   *
   * @param line from normalized token file
   * @return
   */
  def tokensFromLine(line: String): Array[String] = {
    line.trim.split(" ").grouped(2).map(_.head).toArray
  }

  /**
   * Create a default 1-to-1 alignment for the space-delimited tokens of a sentence.
   *
   * @param sent space-delimited tokens
   * @return 1-to-1 alignment for the tokens in sent
   */
  def oneToOne(sent: String): Alignment = {
    new Alignment(
      (sent.split(" ").zipWithIndex.map{ case (_, i) =>
        (i+1, i+1)
      }.toSeq :+ (0,0)).toMap
    )
  }

}