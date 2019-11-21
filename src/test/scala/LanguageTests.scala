package xyz.hyperreal.yola

import utest._

object LanguageTests extends TestSuite {

  import Testing._

  val tests = Tests {
    test("values") {
      assert(runResult("[]") == Nil)
      assert(runResult("3::[]") == List(3))
      assert(runResult("3::4::[]") == List(3, 4))
      assert(runResult("3::4::[5]") == List(3, 4, 5))
    }

    test("destructuring") {
      assert(
        runCapture("""
                     |val a :: (x@b :: c :: []) = [3, 4, 5]
                     |
                     |println( a, b, c, x )
                     |""".stripMargin) ==
          """
            |3, 4, 5, [4, 5]
            |""".stripMargin.trim)
    }

    test("arithmetic") {
      assert(runResult("1 + 2") == 3)
    }

    test("functions") {
      assert(runResult("""
                         |def f(x) = x + 3
                         |
                         |f(4)
                         |""".stripMargin) == 7)
    }

    test("variables") {
      assert(runCapture("""
                          |val a = 3
                          |
                          |println( a + 1 )
                          |""".stripMargin) == "4")
    }

    test("for") {
      assert(
        runCapture("""
                     |for i <- 1..4 if i%2 == 0; j <- 5..8 if j%2 == 1
                     |  println( i, j )
                     |""".stripMargin) ==
          """
            |2, 5
            |2, 7
            |4, 5
            |4, 7
            |""".stripMargin.trim)
      assert(runCapture("""
                          |val l =
                          |  for i <- 1..4 if i%2 == 0; j <- 5..8 if j%2 == 1
                          |    yield (i, j)
                          |
                          |println( l )
                          |""".stripMargin) == "Vector((2, 5), (2, 7), (4, 5), (4, 7))")
    }

    test("lexical scope") {
      assert(
        runCapture("""
                     |val a = 3
                     |
                     |if true
                     | val a = 5
                     |
                     | println( a )
                     |
                     |println( a )
                     |""".stripMargin) ==
          """
            |5
            |3
            |""".stripMargin.trim)
      assert(
        runCapture("""
                     |val a = 3
                     |
                     |if false
                     | ()
                     |else
                     | val a = 5
                     |
                     | println( a )
                     |
                     |println( a )
                     |""".stripMargin) ==
          """
            |5
            |3
            |""".stripMargin.trim)
    }
  }

}
