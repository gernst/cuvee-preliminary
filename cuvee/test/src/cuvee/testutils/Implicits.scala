package cuvee.testutils

import cuvee.{Formal, Id, Sort}

/**
 * Little helpers to swiftly write formal expressions and cuvee programs in tests:
 * import cuvee.testutils.Implicits._
 */
object Implicits {
    val indexed = raw"^(.+?)(\d+)?$$".r // +? is reluctant

    private def splitIndex(s: String): (String, Option[Int]) = {
        val m = indexed.pattern.matcher(s)
        if (!m.find()) {
            throw new IllegalArgumentException("What is this: " + s)
        }
        m.group(2) match {
            case null => (s, None)
            case any: String => (m.group(1), Some(Integer.parseInt(any)))
        }
    }

    implicit def id(s: String): Id = {
        val (base, index) = splitIndex(s)
        Id(base, index)
    }

    implicit def sort(s: String): Sort = {
        val (base, index) = splitIndex(s)
        Sort(base, index)
    }

    implicit def formal(f: (String, String)): Formal = Formal(id(f._1), sort(f._2))
}
