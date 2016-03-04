package ithaka.core.normalize

import ithaka.core.model._
import de.bwaldvogel.liblinear._
import java.io.{FileWriter, File}
import io.Source
import scala.Some
import collection.mutable
import collection.mutable.ArrayBuffer


/**
 * An implementation of a text-normalization system based on the paper:
 *
 * Han, Bo, and Timothy Baldwin. "Lexical Normalisation of Short Text Messages: Makn Sens a# twitter." ACL. 2011.
 *
 * This class detects whether an OOV token is a real OOV token or a lexical variant of an IV token.
 *
 * @param modelFile
 * @param dependencyBank
 */
class HanBaldwinIllformedWordDetector(val modelFile: File, val dependencyBank: DependencyBank) extends IllformedWordDetector {

  val model = Model.load(modelFile)

  def isIllformed(token: Token, candidates: Array[Confusable]): Boolean =
    !(candidates.map{ c: Confusable =>
      Linear.predict(model, HanBaldwinIllformedWordDetector.createExemplar(c, dependencyBank) )
    }.forall( _ == -1.0 ))

}

object HanBaldwinIllformedWordDetector {

  /**
   * Helper function
   */
  def distinctNodes(nodes: Seq[FeatureNode]): Array[FeatureNode] = {
    val builder = ArrayBuffer[FeatureNode]()
    val seen = mutable.HashSet[Int]()

    for (t <- nodes) {
      if (!seen(t.getIndex)) {
        builder += t
        seen += t.getIndex
      }
    }

    builder.toArray
  }

  /**
   * Train a model.
   *
   * @param confusionSetGenerator the confusion set generator
   * @param dependencyBank the dependency bank
   * @param cleanCorpus file of the clean data corpus
   * @param modelFile SVM model file
   */
  def train(confusionSetGenerator: ConfusionSetGenerator, dependencyBank: DependencyBank, cleanCorpus: File, modelFile: File) {

    val n = 1000000
    val k = 4

    val writer: FileWriter = new FileWriter(new File("full_data_500k"))

    println("Training with n=%d and k=%d".format(n, k))

    val positiveVectors = mutable.HashSet[String]()
    println("Collecting positive instances...")
    Source.fromFile(cleanCorpus).getLines().take(n).foreach {
      line: String =>
        Tokens.fromSentence(line.split(" ")).par.map {
          token: Token =>
            val f = createExemplar(token, dependencyBank).map(f => "%s:%s".format(f.getIndex.toString, f.getValue.toString)).mkString(" ")
            positiveVectors.add( f )
            writer.write("+1 %s\n".format(f))
        }
    }

    println("Collecting negative instances...")
    Source.fromFile(cleanCorpus).getLines().take(n / k).foreach {
      line: String =>
        Tokens.fromPost(line).par.foreach {
          token: Token =>
            val confusables = confusionSetGenerator.generateConfusables(token).filter(!_.token.equals(token.token)).take(k)
            if (!confusables.isEmpty) {
              confusables.foreach{ confusable: Confusable =>
                val f = createExemplar(confusable, dependencyBank).map(f => "%s:%s".format(f.getIndex.toString, f.getValue.toString)).mkString(" ")
                if (f.equals("0:0.0") || !positiveVectors.contains(f))
                  writer.write("-1 %s\n".format(f))
              }
            }
        }
    }

    writer.close()
  }

  /**
   * Create a single training example.
   *
   * @param token the token to be considered.
   * @param dependencyBank the dependency bank.
   * @return
   */
  def createExemplar(token: Token, dependencyBank: DependencyBank): Array[Feature] = {
    val i = token.position
    val c = token.context

    val f = (i - DependencyBank.context to i + DependencyBank.context).filter(j => j >= 0 && j < c.length && j != i).flatMap {
      j: Int =>
        val s = dependencyBank.getIDsAndScores(token.token, c(j).token, i, j)

        if (s(0)._2 > 0.0)
          Some(new FeatureNode(s(0)._1+2, s(0)._2))
        else if (s(1)._2 > 0.0)
          Some(new FeatureNode(s(1)._1+2, s(1)._2))
        else if (s(2)._2 > 0.0)
          Some(new FeatureNode(s(2)._1+2, dependencyBank.wD * s(2)._2))
        else if (s(3)._2 > 0.0)
          Some(new FeatureNode(s(3)._1+2, dependencyBank.wD * s(3)._2))
        else
          None

    }.sortBy(_.getValue).reverse

    Array[FeatureNode](new FeatureNode(1, f.map(_.getValue).sum)) ++ distinctNodes(f).sortBy(_.getIndex)
  }

}