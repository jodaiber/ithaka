package ithaka.core.model

/**
 * Generates a set of IV token candidates for an OOV token.
 */
trait ConfusionSetGenerator {

  /**
   * Generate a set of IV token candidates for the OOV token.
   *
   * @param token OOV token
   * @return
   */
  def generateConfusables(token: Token): Array[Confusable]

}
