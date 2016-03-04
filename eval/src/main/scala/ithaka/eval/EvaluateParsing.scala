package ithaka.eval

import java.io.{FileWriter, PrintWriter, FileInputStream, File}
import mstparser.io.{DependencyWriter, CONLLWriter, DependencyReader}
import io.Source
import mstparser.DependencyInstance
import ithaka.core.model.{Alignment, Parser, DependencyBank}
import opennlp.tools.postag.{POSModel, POSTaggerME}
import ithaka.core.parse._
import ithaka.core.normalize._
import ithaka.core.tag._
import ithaka.core.Utils
import ithaka.core.normalize.moses.MosesNormalizer
import scala.Some
import edu.stanford.nlp.tagger.maxent.MaxentTagger
import collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * Utility object for evaluating parser setups.
 */
object EvaluateParsing {

  case class CommandLineConfig(
    parserModel: File = null,
    tokens: File = null,
    gold: File = null,
    alignments: File = null,
    outputFolder: File = new File("."),
    dataFolder: File = new File("/data/thesis/")
  )

  /**
   * Output detailed information about a dependency tree for a sentence.
   *
   * @param tokenizedSent the tokenized, space-delimited sentence
   * @param dP predicted dep. tree
   * @param dG gold dep. tree
   * @param aP alignment for the predicted dep. tree
   * @param aG alignment for the gold dep. tree
   */
  def outputTreeDetails(tokenizedSent: String, dP: DependencyInstance, dG: DependencyInstance, aP: Alignment, aG: Alignment) {

    //Unlabeled:
    val mG = for ((i, j) <- dG.heads.zipWithIndex.tail) yield (aG.sourceNode(i), aG.sourceNode(j))
    val mP = for ((i, j) <- dP.heads.zipWithIndex.tail) yield (aP.sourceNode(i), aP.sourceNode(j))

    //Original tokens:
    val oTokens = ("ROOT " + tokenizedSent).split(" ")

    val fns = mG.diff(mP)
    val fps = mP.diff(mG)

    if (!fns.isEmpty || !fps.isEmpty) {
      println("Tokens: " + tokenizedSent)

      printInstance("Gold", dG, Some(aG), Some(tokenizedSent))
      println("Gold alignment: " + aG)

      printInstance("Pred", dP, Some(aP), Some(tokenizedSent))
      println("Pred alignment: " + aP)


      def token(i: Int): String = {
        if (i >= 0)
          oTokens(i)
        else if (i == -1)
          "{}"
        else
          i.toString
      }

      fns.foreach{ case (i, j) =>
        println( "False negative: %s -> %s".format(token(j), token(i)) )
      }
      fps.foreach{ case (i, j) =>
        println( "False positive: %s -> %s".format(token(j), token(i)) )
      }

    } else {
      println("Instance ok.")
    }

  }

  /**
   * Calculate unlabeled precision and recall for a single tree.
   *
   * @param tokenizedSent the tokenized, space-delimited sentence
   * @param dP predicted dep. tree
   * @param dG gold dep. tree
   * @param aP alignment for the predicted dep. tree
   * @param aG alignment for the gold dep. tree
   * @return
   */
  def evaluateTreeUnlabeled(tokenizedSent: String, dP: DependencyInstance, dG: DependencyInstance, aP: Alignment, aG: Alignment): (Int, Int, Int) = {
    val mG = for ((i, j) <- dG.heads.zipWithIndex.tail) yield (aG.sourceNode(i), aG.sourceNode(j))
    val mP = for ((i, j) <- dP.heads.zipWithIndex.tail) yield (aP.sourceNode(i), aP.sourceNode(j))

    //TP, FP, FN
    (mG.intersect(mP).size, mP.diff(mG).size, mG.diff(mP).size)
  }


  /**
   * Calculate labeled precision and recall for a single tree.
   *
   * @param tokenizedSent the tokenized, space-delimited sentence
   * @param dP predicted dep. tree
   * @param dG gold dep. tree
   * @param aP alignment for the predicted dep. tree
   * @param aG alignment for the gold dep. tree
   * @return
   */

  def evaluateTreeLabeled(tokenizedSent: String, dP: DependencyInstance, dG: DependencyInstance, aP: Alignment, aG: Alignment): (Int, Int, Int) = {
    val mG = for ((i, j) <- dG.heads.zipWithIndex.tail) yield (aG.sourceNode(i), dG.deprels(j), aG.sourceNode(j))
    val mP = for ((i, j) <- dP.heads.zipWithIndex.tail) yield (aP.sourceNode(i), dP.deprels(j), aP.sourceNode(j))

    //TP, FP, FN
    (mG.intersect(mP).size, mP.diff(mG).size, mG.diff(mP).size)
  }


  /**
   * Sum the scores for all trees in the sample x.
   *
   * @param x sample of individual precision and recall counts
   * @return
   */
  def scores(x: Seq[(Int, Int, Int)]): (Double, Double, Double) = {
    val tp = x.map(_._1).sum
    val fp = x.map(_._2).sum
    val fn = x.map(_._3).sum

    val precision = tp / (tp+fp).toDouble
    val recall = tp / (tp+fn).toDouble

    val f1 = (2*precision*recall)/(precision+recall)

    (precision, recall, f1)
  }

  //Bootstrap size for significance testing
  val b = 1000000

  //Approximate randomization sampling limit
  val R = 10000


  /**
   * Randomly sample from the unlabeled and labeled counts xA and xB.
   *
   * @param xA unlabeled counts for the corpus
   * @param xB labeled counts for the corpus
   * @return
   */
  def sampleFromCounts(xA: IndexedSeq[(Int, Int, Int)], xB: IndexedSeq[(Int, Int, Int)]) = xA.map{_ =>
    val r = Random.nextInt(xA.size)
    (xA(r), xB(r))
  }.unzip

  /**
   * Parse the sentences.
   *
   * @param goldInput gold input
   * @param parser dependency parser object
   * @return
   */
  def p(goldInput: Seq[(DependencyInstance, String, Alignment)], parser: Parser): Seq[(String, DependencyInstance, DependencyInstance, Alignment, Alignment)] =
    goldInput.map {
      case (dG: DependencyInstance, tokenizedSent: String, aG: Alignment) =>
        val (dP, aP) = parser.parseWithAlignment( tokenizedSent )
        (tokenizedSent, dP, dG, aP, aG)
    }

  /**
   * Get the counts for each tree (wihtout dependency relation)
   *
   * @param p the parsed corpus
   * @return
   */
  def countsUnlabeled(p: Seq[(String, DependencyInstance, DependencyInstance, Alignment, Alignment)]): IndexedSeq[(Int, Int, Int)] =
    p.map{ case (tokenizedSent: String, dP: DependencyInstance, dG: DependencyInstance, aP: Alignment, aG: Alignment) =>
      evaluateTreeUnlabeled(tokenizedSent, dP, dG, aP, aG)
    }.toIndexedSeq


  /**
   * Get the counts for each tree (including dependency relation)
   *
   * @param p the parsed corpus
   * @return
   */
  def countsLabeled(p: Seq[(String, DependencyInstance, DependencyInstance, Alignment, Alignment)]): IndexedSeq[(Int, Int, Int)] =
    p.map{ case (tokenizedSent: String, dP: DependencyInstance, dG: DependencyInstance, aP: Alignment, aG: Alignment) =>
      evaluateTreeLabeled(tokenizedSent, dP, dG, aP, aG)
    }.toIndexedSeq


  /**
   * Calculate the p-value based on the corpus counts of the baseline and system A.
   *
   * @param countsBaseline counts for the baseline
   * @param countsA counts for system A
   * @return
   */
  def pValue(countsBaseline: IndexedSeq[(Int, Int, Int)], countsA: IndexedSeq[(Int, Int, Int)]): Double = {
    val (_, _, f1B) = scores(countsBaseline)
    val (_, _, f1A) = scores(countsA)

    val deltaX = f1A - f1B

    val s = (0 to b).count{_ =>
      val (sampleA, sampleB) = sampleFromCounts(countsA, countsBaseline)
      val f1A = scores(sampleA)._3
      val f1B = scores(sampleB)._3

      (f1A-f1B) > 2*deltaX
    }

    s / b.toDouble
  }

  /**
   * Evaluate the performance of a parser given a baseline.
   *
   * @param name name of the parser
   * @param pA parser results on corpus
   * @param pBOption optional baseline
   * @param texWriter verbose output
   * @param csvWriter verbose output
   * @param verbose whether verbose output is activated
   */
  def evaluateParser(name: String, pA: Seq[(String, DependencyInstance, DependencyInstance, Alignment, Alignment)], pBOption: Option[Seq[(String, DependencyInstance, DependencyInstance, Alignment, Alignment)]], texWriter: PrintWriter, csvWriter: PrintWriter, verbose: Boolean = false) {

    //Calculate F1 score, delta(x) and bootstrap:
    val countsU = countsUnlabeled(pA)
    val (precisionUnlabeled, recallUnlabeled, f1Unlabeled) = scores(countsU)

    //Calculate labeled F1:
    val countsL = countsLabeled(pA)
    val (precisionALabeled, recallALabeled, f1ALabeled) = scores(countsL)

    val (pUnlabeled, pLabeled) = pBOption match {
      case Some(pB) => (pValue(countsUnlabeled(pB), countsU), pValue(countsLabeled(pB), countsL))
      case None => (0.0, 0.0)
    }

    println("Parser: %s".format(name))
    println("Precision: %f".format(precisionUnlabeled))
    println("Recall: %f".format(recallUnlabeled))
    println("F1: %f".format(f1Unlabeled))
    println("p-value (bootstrap): ", pUnlabeled)

    println("Precision (labeled): %f".format(precisionALabeled))
    println("Recall (labeled): %f".format(recallALabeled))
    println("F1 (labeled): %f".format(f1ALabeled))
    println("p-value (bootstrap): ", pLabeled)

    println("\n" * 2)

    def printPValue(p: Double): String = if (p < 0.05) "\\sig" else "\\notsig"

    texWriter.println("%s & %.2f%s & %.2f%s \\\\".format(name, f1Unlabeled*100.0, printPValue(pUnlabeled), f1ALabeled*100.0, printPValue(pLabeled)))
    csvWriter.println("%s\t%.6f\t%.6f\t%.6f\t%.6f\t%.6f\t%.6f".format(name, precisionUnlabeled, recallUnlabeled, f1Unlabeled, precisionALabeled, recallALabeled, f1ALabeled))
  }


  /**
   * Evaluate parsers on an aligned corpus.
   *
   * @param parsers sequence of parsers to be evaluated
   * @param baselineParser baseline parser
   * @param goldFile gold dependencies file
   * @param tokensFile original tokens file
   * @param alignmentFile corresponding alignment file
   * @param texWriter verbose output
   * @param csvWriter verbose output
   * @param evalDirectory output directory for evaluation results
   * @param verbose whether verbose output is activated
   */
  def evaluateAligned(parsers: Seq[Parser], baselineParser: Option[Parser], goldFile: File, tokensFile: File, alignmentFile: File, texWriter: PrintWriter, csvWriter: PrintWriter, evalDirectory: File, verbose: Boolean = false) {

    val goldReader = DependencyReader.createDependencyReader("CONLL")
    val labeled = goldReader.startReading( goldFile.getAbsolutePath )

    val goldInput = Utils.zip3(
      goldReader.toList,
      Source.fromFile(tokensFile, "utf-8").getLines().toList,
      Source.fromFile(alignmentFile).getLines().map(Alignment.fromLine).toList
    ).asInstanceOf[Seq[(DependencyInstance, String, Alignment)]]

    //Evaluate the baseline:
    val pB = baselineParser.map( baselineParser => p(goldInput, baselineParser))
    baselineParser match {
      case Some(parser) => evaluateParser(parser.name, pB.get, None, texWriter, csvWriter, verbose=verbose)
      case None =>
    }

    //Evaluate all others:
    parsers.foreach{ parser: Parser =>
      val pA = p(goldInput, parser)

      evaluateParser(parser.name, pA, pB, texWriter, csvWriter, verbose=verbose)

      //Verbose output for debugging purposes:
      if (verbose) {
        val evalTokens = new File(evalDirectory, "eval_tokens/")
        evalTokens.mkdir()

        val tokenWriter = new PrintWriter( new File(evalTokens + parser.name) )
        pA.foreach{ case (tokenizedSent: String, dP: DependencyInstance, dG: DependencyInstance, aP: Alignment, aG: Alignment) =>
          tokenWriter.println( dP.forms.tail.mkString(" ") )
          outputTreeDetails(tokenizedSent, dP, dG, aP, aG)
        }
        tokenWriter.close()
      }

    }
  }

  /**
   * Evaluate accuracy for non-aligning parsers/corpora.
   *
   * This method is based on the Scala implementation of the MST parser (https://github.com/travisbrown/mstparser).
   *
   * @param parser the parser to evaluate
   * @param goldFile gold depencies file
   * @param tokensFile original tokens file
   * @param texWriter verbose output
   * @param csvWriter verbose output
   * @param writeResults whether to write results out
   * @param verbose whether to produce verbose outputs
   * @param useGoldTokens whether to use the gold tokens from the normalized version (if available)
   */
  def evaluateAccuracy(parser: Parser, goldFile: File, tokensFile: File, texWriter: PrintWriter, csvWriter: PrintWriter, writeResults: Boolean = false, verbose: Boolean = false, useGoldTokens: Boolean = false) {

    val goldReader = DependencyReader.createDependencyReader("CONLL")
    val labeled = goldReader.startReading( goldFile.getAbsolutePath )

    var predWriter: DependencyWriter = null
    if (writeResults) {
      predWriter = new CONLLWriter(true)
      predWriter.startWriting(new File(tokensFile, ".pred").getAbsolutePath)
    }

    var iID = -1
    val tokensForID = Source.fromFile(tokensFile, "utf-8").getLines().toList.map(l =>
      l.split("\t") match {
        case Array(id: String, tokens: String) => (id.toInt, tokens)
        case Array(tokens: String) => iID += 1; (iID, tokens)
      }
    ).groupBy(_._1).mapValues(_.map(_._2))

    val parsesForID = tokensForID.map{
      case (id: Int, tokenizedSents: List[String]) =>
        (id, tokenizedSents.flatMap{ tokenizedSent: String =>
          parser.parseNBest( tokenizedSent )
        })
    }.toMap

    val (corr, corrL, corrSent, corrSentL) =
      goldReader.zipWithIndex.foldLeft(0, 0, 0, 0) {
        case ((c, cL, cs, csL), (goldInstance, i)) =>
          val parses = if (useGoldTokens)
            parser.parseNBest( goldInstance.forms.tail.mkString(" ") )
          else
            parsesForID( i )

          val best = parses.map({
            predInstance: DependencyInstance =>

              if (verbose) {
                printInstance("Gold", goldInstance)
                printInstance("Pred", predInstance)
              }

              goldInstance.difference(predInstance) match {
                case Some((d, dL)) => (
                  predInstance,
                  c + goldInstance.size - d,
                  cL + goldInstance.size - dL,
                  cs + (if (d == 0) 1 else 0),
                  csL + (if (dL == 0) 1 else 0)
                  )
                case None =>
                  println("Lengths do not match on sentence " + i)
                  (predInstance, c, cL, cs, csL)
              }
          }).sortBy(_._2).last

          if (writeResults)
            predWriter.write(best._1)

          (best._2, best._3, best._4, best._5)
      }

    if (writeResults)
      predWriter.finishWriting()

    val name = parser.name
    println("Parser: %s".format(name))
    println("Tokens: " + goldReader.tokenCount)
    println("Correct: " + corr)
    println("Unlabeled Accuracy: " + corr.toDouble / goldReader.tokenCount)
    println("Unlabeled Complete Correct: " + corrSent.toDouble / goldReader.instanceCount)
    if (labeled) {
      println("Labeled Accuracy: " + corrL.toDouble / goldReader.tokenCount)
      println("Labeled Complete Correct: " + corrSentL.toDouble / goldReader.instanceCount)
    }
    print("\n")
    println("%s & %d / %d & %.6f & %.6f \\\\".format(name, corr, goldReader.tokenCount, corr.toDouble / goldReader.tokenCount, corrL.toDouble / goldReader.tokenCount))
    print("\n" * 4)
  }

  /**
   * Verbose method to print details abut a dependency instance.
   *
   * @param title title of the instance
   * @param instance the dependency instance
   * @param alignment the alignment object
   * @param originalTokens the original tokens
   */
  def printInstance(title: String, instance: DependencyInstance, alignment: Option[Alignment] = None, originalTokens: Option[String] = None) {

    println("#" * 20)
    println(title)
    println("#" * 20)

    for (v <- List(instance.forms, instance.lemmas, instance.postags, instance.cpostags, instance.deprels, instance.heads))
      if (v != null)
        println(v.mkString(" "))

    if (alignment.isDefined) {
      val a = alignment.get
      val oTokens = ("ROOT " + originalTokens.get).split(" ")

      def token(i: Int): String = {
        if (i >= 0)
          oTokens(i)
        else if (i == -1)
          "{}"
        else
          i.toString
      }

      for (i <- 1 until instance.forms.size) {
        val h = instance.heads(i)
        println( "%s -> %s (%s)".format(instance.forms(i), token( a.sourceNode(h) ), instance.forms(h) ) )
      }

    }

    println("#" * 20)
    println("\n")

  }


  def main(args: Array[String]) {

    val parser = new scopt.OptionParser[CommandLineConfig]("eval") {
      head("eval", "1.0")

      arg[File]("<model>") required() valueName("<file>") action { (x, c) =>
        c.copy(parserModel = x) } text("options file of the parser model")

      arg[File]("<tokens>") required() valueName("<file>") action { (x, c) =>
        c.copy(tokens = x) } text("input tokens")

      arg[File]("<gold>") required() valueName("<file>") action { (x, c) =>
        c.copy(gold = x) } text("gold file in CoNLL format")

      opt[File]('a', "alignments") action { (x, c) =>
        c.copy(alignments = x) } text("alignments file")

      opt[File]('o', "output") action { (x, c) =>
        c.copy(outputFolder = x) } text("output folder for results")

      opt[File]('d', "datafolder") action { (x, c) =>
        c.copy(dataFolder = x) } text("folder containing various models")

    }

    val options = parser.parse(args, CommandLineConfig()) match {
      case Some(config) => config
      case None => System.exit(1); null
    }

    //Base Parser
    val dp = MSTParser.fromOptionsFile(options.parserModel)

    //Coarse taggers:
    val arkTagger = new ArkTagger("lib/model.20120919")

    val arkTaggerPDT = new ArkFineTagger("lib/model.ritter_ptb_alldata_fixed.20130723")

    //Fine Taggers:
    val stanfordTagger = new StanfordFirstBestTagger(new MaxentTagger("lib/stanford/pos-tagger/english-bidirectional/english-bidirectional-distsim.tagger"))
    val openNLPTagger = new NBestSequenceTagger(new POSTaggerME(new POSModel(new FileInputStream("lib/en-pos-maxent.bin"))))

    val maxMarkovTagger = DiscriminativeMarkovTagger.fromFiles(new File("lib/wsj.lexicon"), new File("lib/wsj.ngrams"), new POSModel(new FileInputStream("lib/en-pos-maxent.bin")))

    val nBestTaggers = List( arkTaggerPDT )

    //Text normalizers:
    val dependencyBank = DependencyBank.load(new File(options.dataFolder, "dependency_bank/deps_ukwac.t10.model"))
    val generator = HanBaldwinConfusionSetGenerator.fromDictionary(new File(options.dataFolder, "en.dict"), dependencyBank, new File(options.dataFolder, "lm_clean_twitter"))
    val detector  = new HanBaldwinIllformedWordDetector(new File(options.dataFolder, "word_detection.svm_model"), dependencyBank)
    val normalizer = new HanBaldwinTokenNormalizer(generator, detector)

    val dictionaryNormalizer = DictionaryNormalizer.fromFile(new File(options.dataFolder, "han_normalization_dictionary.txt"))
    val mosesAndDictNormalizer = new CombinedNormalizer(new MosesNormalizer(), dictionaryNormalizer)

    //Gold tagger and normalizer:
    val goldTagger = new GoldTagger(options.tokens, options.gold)
    val goldTokenNormalizer = new GoldTokenNormalizer(options.tokens, options.gold)


    val parsersToEvaluate: ArrayBuffer[(Option[Parser], List[Parser])] = ArrayBuffer()

    def parsingConfiguration(parsers: List[Parser], baseline: Option[Parser] = None) {
      parsersToEvaluate.append( (baseline, parsers) )
    }

    //Basic POS tagger evaluation:
    parsingConfiguration(
      nBestTaggers.flatMap(tagger =>
        List(
          new MSTParser( dp, new DefaultJointTagger(tagger) ),
          new MSTParser( dp, new GuessingJointTagger(tagger, arkTagger) )
        )
      ),
      baseline=Some(new MSTParser( dp, new DefaultJointTagger(stanfordTagger) ))
    )

    //Unsupervised normalization:
    List(new DefaultJointTagger(goldTagger), new DefaultJointTagger(stanfordTagger)).foreach{
      tagger: JointTagger => {

        if (options.alignments == null) {

          parsingConfiguration(List(
            new MSTParser( dp, new DefaultJointTagger(goldTagger) )
          ))

          parsingConfiguration(
            List(
              new FirstBestNormalizationParser( dp, goldTokenNormalizer, tagger),

              //1:
              new OracleNBestParser( dp, normalizer, tagger, n = 1 ),
              new OracleNBestParser( dp, normalizer, tagger, n = 1, includeOriginalToken = true ),

              //2:
              new OracleNBestParser( dp, normalizer, tagger, n = 2 ),
              new OracleNBestParser( dp, normalizer, tagger, n = 2, includeOriginalToken = true ),

              //5:
              new OracleNBestParser( dp, normalizer, tagger, n = 5 ),
              new OracleNBestParser( dp, normalizer, tagger, n = 5, includeOriginalToken = true ),

              //10:
              new OracleNBestParser( dp, normalizer, tagger, n = 10 ),
              new OracleNBestParser( dp, normalizer, tagger, n = 10, includeOriginalToken = true ),

              new FirstBestByParseScoreParser( dp, normalizer, tagger ),
              new FirstBestByParseScoreParser( dp, normalizer, tagger, includeOriginalToken = false ),

              new FirstBestByBothScoresParser( dp, normalizer, tagger ),
              new FirstBestByBothScoresParser( dp, normalizer, tagger, includeOriginalToken = true )
            ),
            baseline=Some(new MSTParser( dp, new DefaultJointTagger(stanfordTagger) ))
          )

        } else {

          //MT text normalization:
          parsingConfiguration(
            List(
              new TextNormalizerParser(dp, new GoldTextNormalizer(options.tokens, options.alignments), new DefaultJointTagger(arkTaggerPDT)).asInstanceOf[Parser],
              new TextNormalizerParser(dp, new MosesNormalizer(), new DefaultJointTagger(arkTaggerPDT)).asInstanceOf[Parser],
              new TwitterParser(new TextNormalizerParser(dp, new MosesNormalizer(), new DefaultJointTagger(arkTaggerPDT))).asInstanceOf[Parser]
            ),
            baseline=Some(new MSTParser(dp, new DefaultJointTagger(arkTaggerPDT)).asInstanceOf[Parser])
          )


          //Twitter-specific parser: gold normalization
          parsingConfiguration( List(arkTaggerPDT).map(tagger =>
            new TwitterParser( new TextNormalizerParser(dp, new GoldTextNormalizer(options.tokens, options.alignments), new GuessingJointTagger(tagger, arkTagger)) ).asInstanceOf[Parser]
          ))

          parsingConfiguration( List(
            new TwitterParser( new TextNormalizerParser(dp, new GoldTextNormalizer(options.tokens, options.alignments), new DefaultJointTagger(goldTagger)) ).asInstanceOf[Parser]
          ))

          parsingConfiguration( List(arkTaggerPDT).map(tagger =>
            new TwitterParser( new TextNormalizerParser(dp, new GoldTextNormalizer(options.tokens, options.alignments), new DefaultJointTagger(tagger)) ).asInstanceOf[Parser]
          ))

          //Twitter-specific parser: No normalization:
          parsingConfiguration( nBestTaggers.map(tagger =>
            new TwitterParser( new MSTParser(dp, new DefaultJointTagger(tagger)) ).asInstanceOf[Parser]
          ), baseline=Some( new MSTParser(dp, new DefaultJointTagger(stanfordTagger)) )
          )

          parsingConfiguration( List(arkTaggerPDT).map(tagger =>
            new TwitterParser( new TextNormalizerParser(dp, new MosesNormalizer(), new DefaultJointTagger(tagger)) ).asInstanceOf[Parser]
          ), baseline=Some( new MSTParser(dp, new DefaultJointTagger(arkTaggerPDT)) )
          )

        }


        parsingConfiguration(
          List(
            new TextNormalizerParser(dp, new TokenTextNormalizer(normalizer), new DefaultJointTagger(arkTaggerPDT)).asInstanceOf[Parser],
            new TextNormalizerParser(dp, dictionaryNormalizer, new DefaultJointTagger(arkTaggerPDT)).asInstanceOf[Parser]
          ), baseline=Some(new MSTParser(dp, new DefaultJointTagger(arkTaggerPDT)))
        )


        parsingConfiguration( List(
          new TwitterParser(new TextNormalizerParser(dp, dictionaryNormalizer, new DefaultJointTagger(openNLPTagger))).asInstanceOf[Parser],
          new TwitterParser(new TextNormalizerParser(dp, mosesAndDictNormalizer, new DefaultJointTagger(openNLPTagger))).asInstanceOf[Parser],

          new TwitterParser(new TextNormalizerParser(dp, dictionaryNormalizer, new DefaultJointTagger(arkTaggerPDT))).asInstanceOf[Parser],
          new TwitterParser(new TextNormalizerParser(dp, mosesAndDictNormalizer, new DefaultJointTagger(arkTaggerPDT))).asInstanceOf[Parser]
                  ),baseline=None)

        //Simple unsupervised evaluation:
        parsingConfiguration(List(
          new TextNormalizerParser(dp, new TokenTextNormalizer(normalizer), new DefaultJointTagger(arkTaggerPDT)).asInstanceOf[Parser],
          new TextNormalizerParser(dp, new TokenTextNormalizer(normalizer), new DefaultJointTagger(stanfordTagger)).asInstanceOf[Parser],
          new TextNormalizerParser(dp, dictionaryNormalizer, new DefaultJointTagger(arkTaggerPDT)).asInstanceOf[Parser],
          new TextNormalizerParser(dp, dictionaryNormalizer, new DefaultJointTagger(stanfordTagger)).asInstanceOf[Parser]
        ), baseline=Some(new MSTParser(dp, new DefaultJointTagger(stanfordTagger)).asInstanceOf[Parser])
        )

      }
    }


    val texWriter = new PrintWriter(new File(options.outputFolder, "eval.tex"))
    val csvWriter = new PrintWriter(new File(options.outputFolder, "eval.csv"))

    if (options.alignments != null) {

      parsersToEvaluate.foreach {
        case (baseline: Option[Parser], parsers: List[Parser]) =>
          evaluateAligned(parsers, baseline, options.gold, options.tokens, options.alignments, texWriter, csvWriter, options.outputFolder, verbose = false)
      }

    } else {

      parsersToEvaluate.foreach {
        case (baseline: Option[Parser], parsers: List[Parser]) =>
          (parsers ++ baseline.toList).foreach{ parser: Parser =>
            evaluateAccuracy(parser, options.gold, options.tokens, texWriter, csvWriter, verbose = false)
          }
      }

    }

    texWriter.close()
    csvWriter.close()

  }

}
