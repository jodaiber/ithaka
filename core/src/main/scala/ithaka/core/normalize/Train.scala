package ithaka.core.normalize

import java.io.{FileWriter, File}
import io.Source
import ithaka.core.model.{DependencyBank, Tokens}
import edu.berkeley.nlp.lm.io.{MakeLmBinaryFromArpa, MakeKneserNeyArpaFromText}

/**
 * Utility object for training the required models for the Han and Baldwin normalizer.
 */
object Train {

  def makeLM(text: File) {

    //Tokenizing:
    val tokenized = new File(text.getAbsolutePath+".tokenized")

    if (!tokenized.exists()) {
      val fwriter: FileWriter = new FileWriter(tokenized)
      Source.fromFile( text ).getLines().foreach({
        line: String =>
          fwriter.write(Tokens.fromPost( line ).mkString(" ") + "\n")
      })
      fwriter.close()
    }

    val arpa = new File(text.getAbsolutePath+".arpa")
    val blm = new File(text.getAbsolutePath+".blm")

    if (!arpa.exists())
      MakeKneserNeyArpaFromText.main(Array[String]("3", arpa.getAbsolutePath, tokenized.getAbsolutePath))

    MakeLmBinaryFromArpa.main(Array[String](arpa.getAbsolutePath, blm.getAbsolutePath))
  }



  def main(args: Array[String]) {
    //DependencyBank.store( DependencyBank.fromSortedFile(new File("/data/thesis/dependency_bank/deps_ukwac.counts_t10")), new File("/data/thesis/dependency_bank/deps_ukwac.t10.model") )

    //makeLM(new File("/data/thesis/en_iv"))
    val dependencyBank = DependencyBank.load(new File("/data/thesis/dependency_bank/deps_ukwac.t10.model"))
    val generator = HanBaldwinConfusionSetGenerator.fromDictionary(new File("/data/thesis/en.dict"), dependencyBank, new File("/data/thesis/lm_clean_twitter"))
    HanBaldwinIllformedWordDetector.train(generator, dependencyBank, new File("/data/thesis/en_iv"), new File("/data/thesis/word_detection.svm_model"))
  }
}
