package xyz.hyperreal.yola

import utest._

object YolaExamples extends TestSuite {

  import Testing._

  val tests = Tests {
    test("fibonacci 1") {
      assert(
        runCapture(
          """
            |def fib( n )
            |  var result = []
            |  var a = 0
            |  var b = 1
            |
            |  while a < n
            |    result += a
            |    a, b = b, a + b
            |
            |  result
            |
            |println( fib(10) )
			""".stripMargin.trim
        ) == "[0, 1, 1, 2, 3, 5, 8]")
    }

  }
}
