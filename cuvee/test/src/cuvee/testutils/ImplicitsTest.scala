package cuvee.testutils

import cuvee.Id
import cuvee.test.TestSuite

class ImplicitsTest extends TestSuite {
  test("index is handled in implicit string to Id conversion") {
    var id: Id = Implicits.id("basic")
    assertEquals(id, Id("basic", None))

    id = Implicits.id("pre12")
    assertEquals(id, Id("pre", Some(12)))

    id = Implicits.id("pre123post")
    assertEquals(id, Id("pre123post", None))

    id = Implicits.id("pre123mid56")
    assertEquals(id, Id("pre123mid", Some(56)))
  }
}
