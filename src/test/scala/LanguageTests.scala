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
