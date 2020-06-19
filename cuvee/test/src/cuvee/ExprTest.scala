package cuvee

import cuvee.test.TestSuite
import cuvee.testutils.Implicits._

object ExprTest extends TestSuite {
  test("PostChooseFree") {
    val post = Post(WP, p"(choose (some_var) (f some_var))", e"true")
    assert(post.free.contains("some_var"))
  }
}
