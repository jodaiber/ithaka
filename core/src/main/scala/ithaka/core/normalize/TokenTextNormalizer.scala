package ithaka.core.normalize

import ithaka.core.model._
import java.io.File

class TokenTextNormalizer(tokenNormalizer: TokenNormalizer) extends TextNormalizer {

  def firstBest(tokenizedString: String): (Array[String], Alignment) =
    (tokenNormalizer.firstBest(Tokens.fromTokenizedString(tokenizedString)).map(_.token), Alignment.oneToOne(tokenizedString))

}

object TokenTextNormalizer {

  def main(args: Array[String]) {

    val dependencyBank = DependencyBank.load(new File("/data/thesis/dependency_bank/deps_ukwac.t10.model"))
    val generator = HanBaldwinConfusionSetGenerator.fromDictionary(new File("/data/thesis/en.dict"), dependencyBank, new File("/data/thesis/lm_clean_twitter"))
    val detector  = new HanBaldwinIllformedWordDetector(new File("/data/thesis/word_detection.svm_model"), dependencyBank)
    val normalizer = new HanBaldwinTokenNormalizer(generator, detector)

    val textNormalizer = new TokenTextNormalizer(normalizer)

    val t: (Array[String], Alignment) = textNormalizer.firstBest("wat is dis ?")
    println( t._1.mkString(" ") )
    println( t._2 )

  }


}