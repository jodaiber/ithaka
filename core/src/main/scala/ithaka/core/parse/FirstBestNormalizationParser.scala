package ithaka.core.parse

import ithaka.core.model.{FineTagger, TokenNormalizer}
import mstparser.DependencyParser
import ithaka.core.tag.JointTagger

/**
 * Simple parser ranking n-best normalization by both their normalization only.
 *
 */
class FirstBestNormalizationParser(dp: DependencyParser, normalizer: TokenNormalizer, tagger: JointTagger, includeOriginalToken: Boolean = false)
  extends OracleNBestParser(dp, normalizer, tagger, includeOriginalToken=includeOriginalToken, n=1)