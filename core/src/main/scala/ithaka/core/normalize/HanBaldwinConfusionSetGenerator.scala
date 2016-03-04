package ithaka.core.normalize

import com.infiauto.datastr.auto.{LevenshteinAutomaton, DictionaryAutomaton}
import scala.collection.JavaConversions._
import org.apache.commons.codec.language.DoubleMetaphone
import com.infiauto.datastr.MultiMap
import java.io.File
import io.Source
import breeze.linalg.DenseVector
import org.apache.commons.lang3.StringUtils
import ithaka.core.Utils._
import edu.berkeley.nlp.lm.io.LmReaders
import breeze.{linalg, numerics}
import ithaka.core.model.{DependencyBank, Confusable, Token, Tokens, ConfusionSetGenerator}
import edu.berkeley.nlp.lm.NgramLanguageModel

/**
 * An implementation of a text-normalization system based on the paper:
 *
 * Han, Bo, and Timothy Baldwin. "Lexical Normalisation of Short Text Messages: Makn Sens a# twitter." ACL. 2011.
 *
 * This class generates a set of confusables for an OOV token candidate.
 *
 * @param tc maximum edit distance for IV token candidates
 * @param tp maximum metaphone edit distance for IV token candidates
 * @param ivTokens the dictionary of in-vocabulary tokens
 * @param dependencyBank a dependency-based bag-of-words model
 * @param lmFolder folder containing the language model file
 */
class HanBaldwinConfusionSetGenerator(val tc: Int, val tp: Int, val ivTokens: Set[String], dependencyBank: DependencyBank, lmFolder: File)
  extends ConfusionSetGenerator {

  //Language model:
  //val lm = LmReaders.readGoogleLmBinary(new File(lmFolder, "blm").getAbsolutePath, new File(lmFolder, "vocab_cs.gz").getAbsolutePath)
  println("Loading LM...")
  val lm: NgramLanguageModel[String] = LmReaders.readLmBinary(new File(lmFolder, "lm.blm").getAbsolutePath)
  println("Done.")

  //Edit distance based lookup:
  println("Building exact FSA...")
  val ivTokenDictionary = new DictionaryAutomaton(ivTokens.toList)
  val ivTokenLevenshteinFSA = LevenshteinAutomaton.newInstance(tc)
  println("Done.")


  //Double metaphone based lookup:
  println("Building phonological FSA...")
  val metaphone = new DoubleMetaphone()
  val tokenForMetaphone = ivTokens.groupBy( metaphone.doubleMetaphone(_) )

  val ivMetaphoneDictionary = new DictionaryAutomaton(tokenForMetaphone.keys.toList)
  val ivMetaphoneLevenshteinFSA = LevenshteinAutomaton.newInstance(tp)
  println("Done.")

  /**
   * Generate a set of IV token candidates for the OOV token.
   *
   * @param token OOV token
   * @return
   */
  def generateConfusables(token: Token): Array[Confusable] = {

    val normalizedToken = token.token.split("""(?<=(\w))(?!\1)""").map(_.take(3)).mkString("")

    //Pure edit distance candidates: T_c
    val editCandidates = merge(ivTokenLevenshteinFSA.recognize(normalizedToken, ivTokenDictionary))

    //Double Meta-phone distance candidates: T_p
    val metaphoneCandidates =
      merge(ivMetaphoneLevenshteinFSA.recognize(metaphone.doubleMetaphone(normalizedToken), ivMetaphoneDictionary))
      .map( tokenForMetaphone(_) ).flatten

    val allCandidates = (editCandidates ++ metaphoneCandidates)

    val dependencyCounts = allCandidates.map{ confusable: String =>
      (confusable, dependencyBank.dependencyScore(confusable, token))
    }.toMap
    val totalDependencyCount = linalg.softmax(dependencyCounts.values.toSeq)

    val c = token.context
    val i = token.position

    val weights = DenseVector.ones[Double](7)
    weights /= weights.sum

    val candidates = allCandidates.map{ confusable: String =>

      val maxL = math.max(confusable.length, normalizedToken.length).toDouble

      val c = 1/2.0
      val f = DenseVector(

        //Edit distance:
        math.exp( -1 * StringUtils.getLevenshteinDistance(confusable, normalizedToken) ) ,

        //Phoneme-based edit distance:
        math.exp( -1 * StringUtils.getLevenshteinDistance(metaphone.doubleMetaphone(confusable), metaphone.doubleMetaphone(normalizedToken))),

        //Longest common substring:
        lcs(confusable, normalizedToken).length / maxL,

        //The token is a subsequence abbreviation (Cook and Stevenson, 2009):
        if ( normalizedToken.toSet.diff(confusable.toSet).isEmpty && confusable.toSet.diff(normalizedToken.toSet).forall( "aeiouh".contains(_) ) ) c else 0.0,

        //The token is a prefix of the confusable:
        if( confusable.startsWith(normalizedToken) ) c else 0.0,

        //The token is a suffix of the confusable:
        if( confusable.endsWith(normalizedToken) ) c else 0.0,

        //Dependency score:
        dependencyCounts(confusable) / totalDependencyCount

      )

      val score = f.dot(weights)

      new Confusable(confusable, score, token)
    }.toArray.sorted.take(10)

    val lmScores: Map[String, Double] = candidates.map{ confusable: Confusable =>
      val h = (i-2 to i+2).filter( i => i >= 0 && i < c.length ).map( j => if(j == i) confusable.token else c(j).token ).toList
      (confusable.token, lm.scoreSentence( h ).toDouble)
    }.toMap
    val totalLmScore = numerics.logSum(lmScores.values.toSeq)

    val lmWeights = DenseVector[Double](7,1)
    lmWeights /= lmWeights.sum

    candidates.foreach{ c: Confusable =>
      c.score = DenseVector[Double](c.score, math.exp(lmScores(c.token) - totalLmScore)).dot(lmWeights)
    }

    candidates.sorted
  }

  private def merge(mm: MultiMap[Integer, String]): Set[String] =
    mm.keySet().filter{i: Integer => i != null}.toSet.flatMap{ i: Integer => mm.getAll(i) }
}

object HanBaldwinConfusionSetGenerator {

  /**
   * Create a confusion set generator from a dictionary, a dependency bank and a LM.
   *
   * @param dict the IV dictionary
   * @param dependencyBank the dependency bank
   * @param lmFolder the folder containing the LM
   * @return
   */
  def fromDictionary(dict: File, dependencyBank: DependencyBank, lmFolder: File): HanBaldwinConfusionSetGenerator = {
    new HanBaldwinConfusionSetGenerator(2, 1, Source.fromFile(dict).getLines().map(_.trim).toSet, dependencyBank, lmFolder)
  }

}