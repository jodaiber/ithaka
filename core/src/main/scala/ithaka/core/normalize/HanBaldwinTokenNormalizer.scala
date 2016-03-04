package ithaka.core.normalize

import ithaka.core.model._
import java.io.File
import io.Source


/**
 * An implementation of a text-normalization system based on the paper:
 *
 * Han, Bo, and Timothy Baldwin. "Lexical Normalisation of Short Text Messages: Makn Sens a# twitter." ACL. 2011.
 *
 * This class contains the full token normalizer.
 *
 * @param vocabulary IV token dictionary
 * @param generator confusion set generator
 * @param classifier classifier for OOV tokens
 */
class HanBaldwinTokenNormalizer(val vocabulary: Set[String], val generator: HanBaldwinConfusionSetGenerator, val classifier: HanBaldwinIllformedWordDetector) extends TokenNormalizer {

  def this(generator: HanBaldwinConfusionSetGenerator, classifier: HanBaldwinIllformedWordDetector) {
    this(generator.ivTokens, generator, classifier)
  }


  /**
   * Get first-best corrections for noisy text.
   * @param text
   * @return
   */
  def firstBest(text: String): Array[Token] = {
    nBest(text) map(_.head)
  }

  def firstBest(tokens: Array[Token]): Array[Token] = {
    nBest(tokens) map(_.head)
  }

  /**
   * Get n-best corrections for noisy text.
   */
  def nBest(text: String): Array[Array[Token]] = {
    nBest(Tokens.fromPost(text))
  }

  def nBest(tokens: Array[Token]): Array[Array[Token]] = {

    tokens.map { token: Token =>
      if (!vocabulary.contains(token.token)
        && !vocabulary.contains(token.token(0).toLower + token.token.tail)
        && !token.token.matches("""^(Username|Urlname|RT|MT|@|.@|#).*""")
        && !token.token.matches("""(-RRB-|-LRB-|\p{Punct}|[0-9]+).*""")
      ) {
        val confusables = generator.generateConfusables(token)

        if (classifier.isIllformed(token, confusables) && confusables.head.score > 0.3)
          confusables.map(_.asInstanceOf[Token])
        else
          Array[Token](token)

      } else {
        Array[Token](token)
      }
    }
  }

}

object HanBaldwinTokenNormalizer {

  /**
   * Iterate all possible combinations of normalizations.
   *
   * @param text the n-best normalized sentence
   * @return
   */
  def iterateAll(text: Array[Array[Token]]): Array[Array[Token]] = {
    if (text.length == 0)
      text
    else if (text.length == 1)
      text(0).map(t => Array[Token](t)).array
    else
      text(0).flatMap{ token: Token =>
        iterateAll(text.tail) map { tail: Array[Token] =>
          token +: tail
        }
      }
  }

  def main(args: Array[String]) {

    val dependencyBank = DependencyBank.load(new File("/data/thesis/dependency_bank/deps_ukwac.t10.model"))
    val generator = HanBaldwinConfusionSetGenerator.fromDictionary(new File("/data/thesis/en.dict"), dependencyBank, new File("/data/thesis/lm_clean_twitter"))
    val detector  = new HanBaldwinIllformedWordDetector(new File("/data/thesis/word_detection.svm_model"), dependencyBank)
    val normalizer = new HanBaldwinTokenNormalizer(generator, detector)

    val n = if (args.size > 1) args(1).toInt else 1

    Source.fromFile(new File(args(0))).getLines().zipWithIndex.foreach{ case (line: String, i: Int) =>
      (
        if ( n == 1 )
          Array(normalizer.firstBest( Tokens.fromTokenizedString(line.trim) ))
        else{
          val tokens = Tokens.fromTokenizedString(line.trim)
          val nbest: Array[Array[Token]] = normalizer.nBest(tokens)

          if (args.size == 3)
            tokens.zipWithIndex.foreach{
              case (t: Token, i: Int) =>
                if (!nbest(i).contains(t))
                  nbest(i) :+= t
            }

          iterateAll( nbest )
        }

      ).foreach{ ts: Array[Token] =>
        println( "%d\t%s".format(i, ts.map(_.token).mkString(" ")) )
      }
    }

  }

}