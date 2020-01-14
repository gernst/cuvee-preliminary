package cuvee.testutils

import cuvee.{Formal, Id, Sort}

/**
 * Little helpers to swiftly write formal expressions and cuvee programs in tests:
 * import cuvee.testutils.Implicits._
 */
object Implicits {
    implicit def id(s: String): Id = Id(s)

    implicit def formal(f: (String, String)): Formal = Formal(Id(f._1), Sort(f._2))
}
