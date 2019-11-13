package xyz.hyperreal.yola

import utest._

object LanguageTests extends TestSuite {

  import Testing._

  val tests = Tests {
    test("arithmetic") {
      assert(runResult("1 + 2") == 3)
    }

    test("variables") {
      assert(runCapture("""
                          |val a = 3
                          |
                          |println( a + 1 )
                          |""".stripMargin) == "4")
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
