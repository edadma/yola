package xyz.hyperreal.yola

import utest._

object LanguageTests extends TestSuite {
  import Testing._

  val tests = Tests {
    test("values") {
      assert(runResult("[]") == YList(Nil))
      assert(runResult("3::[]") == YList(List(3)))
      assert(runResult("3::4::[]") == YList(List(3, 4)))
      assert(runResult("3::4::[5]") == YList(List(3, 4, 5)))
      assert(runResult("[3]") == YList(List(3)))
      assert(runResult("[3, 4]") == YList(List(3, 4)))
      assert(runResult("[3, 4, 5]") == YList(List(3, 4, 5)))
      assert(runResult("(3, 4)") == YTuple(List(3, 4)))
      assert(runResult("(3, 4, 5)") == YTuple(List(3, 4, 5)))
      assert(runResult("{}") == YMap(Map()))
      assert(runResult("{a: 3}") == YMap(Map(YString("a") -> YNumber(3))))
      assert(
        runResult("{'a': 3, 4: 5, (6 + 7): (8 + 9)}") == YMap(
          Map(YString("a") -> YNumber(3), YNumber(4) -> YNumber(5), YNumber(13) -> YNumber(17))))
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
            |""".stripMargin.trim
      )
      assert(runResult("""
                         |val [a, b] = [3, 4]
                         |
                         |[b, a]
                         |""".stripMargin) == YList(List(4, 3)))
      assert(runResult("""
                         |val (a, b) = (3, 4)
                         |
                         |[b, a]
                         |""".stripMargin) == YList(List(4, 3)))
    }

    test("arithmetic") {
      assert(runResult("1 + 2") == YNumber(3))
    }

    test("functions") {
      assert(runResult("""
                         |def f(x) = x + 3
                         |
                         |f(4)
                         |""".stripMargin) == YNumber(7))
      assert(runResult("""
                         |(x -> x + 3)(4)
                         |""".stripMargin) == YNumber(7))
      assert(runResult("""
                         |def
                         |  map( f, [] ) = []
                         |  map( f, x::xs ) = f(x) :: map( f, xs )
                         |
                         |map( (*2), [3, 4, 5] )
                         |""".stripMargin) == YList(List(6, 8, 10)))
      assert(runResult("""
                         |def
                         |  filter( p, [] ) = []
                         |  filter( p, x::xs )
                         |    | p( x ) = x :: filter( p, xs )
                         |    | else = filter( p, xs )
                         |
                         |filter( (>4), [3, 4, 5, 6] )
                         |""".stripMargin) == YList(List(5, 6)))
      assert(runResult("""
                         |def
                         |  f(x) = 3x
                         |  g(x) = x + 4
                         |
                         |5 ~> g ~> f
                         |""".stripMargin) == YNumber(27))
      assert(runResult("""
                         |def
                         |  foldl( f, z, [] )    =  z
                         |  foldl( f, z, x::xs ) =  foldl( f, f(z, x), xs )
                         |
                         |  sum( l )             =  foldl( (+), 0, l )
                         |  product( l )         =  foldl( (*), 1, l )
                         |
                         |val l = [3, 4, 5]
                         |
                         |[sum(l), product(l)]
                         |""".stripMargin) == YList(List(12, 60)))
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
            |""".stripMargin.trim
      )
    }

    test("for yield") {
      assert(runCapture("""
                          |val l =
                          |  for i <- 1..4 if i%2 == 0; j <- 5..8 if j%2 == 1
                          |    yield (i, j)
                          |
                          |println( l )
                          |""".stripMargin) == "List((2, 5), (2, 7), (4, 5), (4, 7))")
    }

    test("list comprehensions") {
      assert(
        runCapture("""println( [(i, j) | i <- 1..4 if i%2 == 0, j <- 5..8 if j%2 == 1] )""") == "[(2, 5), (2, 7), (4, 5), (4, 7)]"
      )
    }

    test("lexical scope") {
      assert(runResult("""
                         |val a = 4
                         |val b = 5
                         |val c = 6
                         |
                         |def f(x) = x*a
                         |
                         |7 ~> f ~> x -> x + b ~> (+ c)
                         |""".stripMargin) == YNumber(39))
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
            |""".stripMargin.trim
      )
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
            |""".stripMargin.trim
      )
    }
  }
}
