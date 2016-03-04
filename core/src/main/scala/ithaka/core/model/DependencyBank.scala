package ithaka.core.model

import scala.Int
import com.esotericsoftware.kryo.io.{Output, Input}
import io.Source
import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.serializers.JavaSerializer
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap
import java.io.{FileOutputStream, File, FileInputStream}
import scala.collection.JavaConversions._
import org.apache.commons.codec.language.DoubleMetaphone


/**
 * A class storing and providing a dependency-based bag-of-word model.
 */
@SerialVersionUID(1001002)
class DependencyBank extends Serializable {

  val wD = 0.5

  /**
   * Provide the phonetic representation of a string.
   * @param w input string
   * @return phonetic representation of w
   */
  def phon(w: String): String = DependencyBank.metaphone.doubleMetaphone(w)

  //Mapping from dependency relation to ID
  var idForDependency: Object2IntOpenHashMap[(String, String, Int)] = null

  //Score for a dependency relation (by ID)
  var scoreForID: Array[Double] = null

  //Lookup for the phonetic representation of a dependency relation
  var phoneticLookup: Object2IntOpenHashMap[(String, String, Int)] = null

  def this(idForDependency: Object2IntOpenHashMap[(String, String, Int)], countForID: Array[Int], tokenCounts: Object2IntOpenHashMap[String]) {
    this()
    this.idForDependency = idForDependency
    this.scoreForID = Array.ofDim[Double]( countForID.length )

    //Initialize
    val totalPairCount  = countForID.sum
    val nPairs = countForID.length
    val totalTokenCount = tokenCounts.values().toIntArray.sum
    val nTokens = tokenCounts.size

    //Initialize phonetic lookup
    phoneticLookup = new Object2IntOpenHashMap[(String, String, Int)]()
    phoneticLookup.defaultReturnValue(-1)


    val mu = 1.0

    //Calculate the score for each dependency relation
    idForDependency.entrySet().foreach{
      e: java.util.Map.Entry[(String, String, Int), Integer] =>

        //Retrieve the two words and their distance
        val (w1, w2, pos) = e.getKey

        //Calculate the individual counts for each word, smooth with mu
        val logCountXY = math.log( countForID( e.getValue ) )
        val logCountX  = math.log(tokenCounts.getInt(w1) + mu)
        val logCountY  = math.log(tokenCounts.getInt(w2) + mu)

        //Calculate the individual probabilities
        val pXY = logCountXY - math.log( totalPairCount  + (nPairs * mu) )
        val pX  = logCountX  - math.log( totalTokenCount + (nTokens * mu) )
        val pY  = logCountY  - math.log( totalTokenCount + (nTokens * mu) )

        //Calculate the score for the dep. relation using normalized PMI
        scoreForID( e.getValue ) = (((pXY - (pX + pY)) / -pXY) + 1.0) / 2.0 //<- Normalized PMI +1.0 (so the range is 0.0 to 2.0) / 2.0

        //Generate the phonetic rep. version of the relation
        val p = (phon(w1), phon(w2), pos)

        phoneticLookup.getInt(p) match {
          case id: Int if id >= 0 => if (scoreForID(id) < scoreForID(e.getValue)) phoneticLookup.put(p, e.getValue)
          case _ => phoneticLookup.put(p, e.getValue)
        }
    }

  }

  /**
   * Get the ID and the score for two words w1 and w2 at position i and j.
   *
   * @param w1 the first word
   * @param w2 the second word
   * @param i Position of the first word
   * @param j Position of the second word
   * @return
   */
  def getIDAndScore(w1: String, w2: String, i: Int, j: Int): (Int, Double) = {
    idForDependency.getInt((w1, w2, i-j)) match {
      case id: Int if id > 0 => (id, scoreForID(id))
      case _ => (-1, 0.0)
    }
  }


  /**
   * Get the ID and the score for a relation of w1 and w2.
   *
   * @param w1 the first word
   * @param w2 the second word
   * @param i position of the first word
   * @param j position of the second word
   * @return
   */
  def getIDsAndScores(w1: String, w2: String, i: Int, j: Int): Array[(Int, Double)] = {
    Array(
      getIDAndScore(w1, w2, i, j),
      getIDAndScore(w2, w1, j, i),
      phoneticLookup.getInt((phon(w1), phon(w2), i - j)) match {
        case id: Int if(id == -1) => (-1, 0)
        case id: Int => (id, scoreForID(id))
      },
      phoneticLookup.getInt((phon(w2), phon(w1), j - i)) match {
        case id: Int if(id == -1) => (-1, 0)
        case id: Int => (id, scoreForID(id))
      }
    )
  }

  /**
   * Get the total score for a confusable and a token.
   *
   * @param confusable confusable
   * @param token token
   * @return
   */
  def dependencyScore(confusable: String, token: Token): Double = {
    val i = token.position
    val c = token.context

    (i - DependencyBank.context to i + DependencyBank.context).filter(j => j >= 0 && j < c.length && j != i).map{
      j: Int =>
        val s = getIDsAndScores(confusable, c(j).token, i, j)

        if (s(0)._2 + s(1)._2 > 0.0)
          s(0)._2 + s(1)._2
        else
          wD * s(2)._2 + s(3)._2
    }.sum
  }

  def size: Int = scoreForID.length

}

object DependencyBank {

  val metaphone = new DoubleMetaphone()

  //Default context window:
  val context = 3

  //Serialization of the dependency bank:
  val kryo = new Kryo()
  kryo.setRegistrationRequired(true)
  kryo.register(classOf[DependencyBank], new JavaSerializer())

  /**
   * Store the dependency bank object to a file
   * @param dependencyBank dependency bank object
   * @param file output file
   */
  def store(dependencyBank: DependencyBank, file: File) {
    val output = new Output(new FileOutputStream(file))
    kryo.writeClassAndObject(output, dependencyBank)
    output.close()
  }

  /**
   * Load the dependency bank from a file.
   * @param file input file
   * @return
   */
  def load(file: File): DependencyBank = {
    println("Loading dependency bank...")
    val depBank = kryo.readClassAndObject(new Input(new FileInputStream(file))).asInstanceOf[DependencyBank]
    println("Done. n="+depBank.size)
    depBank
  }


  /**
   * Create the dependency bank from a sorted dependency file.
   *
   * @param parsedText file consisting of sorted dependency relations to create a bag-of-word model
   * @return
   */
  def fromSortedFile(parsedText: File): DependencyBank = {

    val idForDependency = new Object2IntOpenHashMap[(String, String, Int)]()
    val tokenCounts = new Object2IntOpenHashMap[String]()
    tokenCounts.defaultReturnValue(0)

    val n = Source.fromFile(parsedText, "latin1").getLines().size
    val countForID = Array.ofDim[Int]( n )

    Source.fromFile(parsedText, "latin1").getLines().zipWithIndex.foreach{ case (line: String, i: Int) => {
      if (!line.trim().isEmpty) {
        val Array(count, dep) = line.trim().split("\t")
        val Array(w1, w2, pos) = dep.split(" ")
        idForDependency.put( (w1, w2, pos.toInt), i )

        val c = count.toInt
        countForID(i) = c
        tokenCounts.put(w1, tokenCounts.getInt(w1) + c)
        tokenCounts.put(w2, tokenCounts.getInt(w2) + c)
      }
    }}

    new DependencyBank(idForDependency, countForID, tokenCounts)
  }

}
