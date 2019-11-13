package xyz.hyperreal.yola

import utest._

object BasicTests extends TestSuite {

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
  }

}
