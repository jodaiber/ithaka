package ithaka.core.normalize.moses

import ithaka.core.model.{Alignment, TextNormalizer, Token, TokenNormalizer}
import org.apache.xmlrpc.client.{XmlRpcClient, XmlRpcClientConfigImpl}
import java.net.URL
import collection.mutable


/**
 * A text normalizer based on the Moses statistical machine translation toolkit.
 *
 * @param url URL of the local Moses server
 */
class MosesNormalizer(url: URL = new URL("http://localhost:8080/RPC2")) extends TextNormalizer {

  //Regular expression for unknown words.
  val unkRegex = """\|UNK""".r

  //Configuration of the local
  val config = new XmlRpcClientConfigImpl()
  config.setServerURL(url)
  val client = new XmlRpcClient()
  client.setConfig(config)


  //Get the raw translations from the Moses server
  protected def getRawTranslation(tokenizedSent: String): (Array[String], Alignment) = {
    val mosesParams = new java.util.HashMap[String, String]()
    mosesParams.put("text", tokenizedSent)
    mosesParams.put("align", "true")
    mosesParams.put("report-all-factors", "false")
    val params = Array[Object](null)
    params(0) = mosesParams

    client.execute("translate", params) match {
      case m: java.util.HashMap[String, Object] => {

        val alignments = m.get("align").asInstanceOf[Array[Object]]

        val alignmentMap = mutable.Map[Int, Int]()
        alignmentMap.put(0, 0)

        for (i <- 0 until alignments.size) {
          val alignment = alignments(i).asInstanceOf[java.util.HashMap[String, Int]]
          alignmentMap.put( alignment.get("tgt-start")+1 , alignment.get("src-start")+1 )

          if (i+1 < alignments.size && alignments(i+1).asInstanceOf[java.util.HashMap[String, Int]].get("tgt-start") != alignment.get("tgt-start")+1 )
            for (j <- alignment.get("tgt-start")+1 to alignments(i+1).asInstanceOf[java.util.HashMap[String, Int]].get("tgt-start"))
              alignmentMap.put( j+1 , alignment.get("src-start")+1 )
        }

          //println("["+alignment.get("src-start")+":"+alignment.get("src-end")+"]["+alignment.get("tgt-start")+":"+alignment.get("tgt-end")+"]")

        (m.get("text").asInstanceOf[String].split("[ ]+").map( unkRegex.replaceAllIn(_, "") ), new Alignment(alignmentMap.toMap))
      }
      case _ => throw new ClassCastException("Wrong type of result from Moses!")
    }
  }


  /**
   * Provide first-best normalization and the alignment for the tokenized, space-delimited
   * input sentence.
   *
   * @param tokenizedString the tokenized input sentence
   * @return
   */
  def firstBest(tokenizedString: String): (Array[String], Alignment) = {
    val t: (Array[String], Alignment) = getRawTranslation(tokenizedString)
    t
  }


}


